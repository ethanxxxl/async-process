#include "async-process.h"

int init_str(struct str *str) {
    str->buf = malloc(sizeof(char) * 256);
    if (str->buf == NULL)
    return -1;

    str->len = 0;
    str->cap = 256;

    return 0;
}

void del_str(struct str *str) {
    free(str->buf);
}

static struct process* allocate_process(int fd_io,
                                        int fd_er,
                                        const char *pts_io_name,
                                        const char *pts_er_name,
                                        int pid) {
    int stdout_ret = 0, stderr_ret = 0;
    char *io_str = NULL, *er_str = NULL;
    size_t io_strlen = strlen(pts_io_name) + 1;
    size_t er_strlen = strlen(pts_er_name) + 1;

    struct process *process = malloc(sizeof(struct process));
    if (process == NULL)
        return NULL;

    stdout_ret = init_str(&process->stdout);
    if (stdout_ret == -1)
        goto FAILED_MALLOC;

    stderr_ret = init_str(&process->stderr);
    if (stderr_ret == -1)
        goto FAILED_MALLOC;

    io_str = malloc(io_strlen * sizeof(char));
    if (io_str == NULL)
        goto FAILED_MALLOC;

    er_str = malloc(er_strlen * sizeof(char));
    if (er_str == NULL)
        goto FAILED_MALLOC;

    memcpy(io_str, pts_io_name, io_strlen);
    memcpy(er_str, pts_er_name, er_strlen);

    process->pts_io_name = io_str;
    process->pts_er_name = er_str;
    process->fd_io = fd_io;
    process->fd_er = fd_er;
    process->pid = pid;

    return process;

FAILED_MALLOC:
    if (process != NULL) free(process);
    if (stdout_ret == -1) del_str(&process->stdout);
    if (stderr_ret == -1) del_str(&process->stderr);
    if (io_str != NULL) free(io_str);
    if (er_str != NULL) free(er_str);
    return NULL;
}

void delete_process(struct process *process) {
    kill(process->pid, 9);
    close(process->fd_io);
    close(process->fd_er);
    del_str(&process->stdout);
    del_str(&process->stderr);
    free(process->pts_io_name);
    free(process->pts_er_name);
    free(process);
}

void my_exit(int status) {
    // exitを使うとatexitで動作に影響を与えられる、これが原因でプロセスを終了できなくなる事があるので使うのを避ける
    // 例えばSDL2はat_exitを使っているせいか、lemのSDL2 frontendでasync_processが動作しなくなっていた
    _exit(status);
}

// opens a PTY and assigns master and slave file descriptors to fdm and fds
// respectively.  Name will be malloced and it is the callers responsibility
// to free name.  On success, return 0.  On fail, returns -1.  All references
// will also be either initialized or set -1/NULL appropriately.
int open_pty(int *fdm, int *fds, char **name) {
    *fdm = -1;
    *fds = -1;
    *name = NULL;
    
    // gets a PTY, and initializes the attached slave PTS.  grantpt and unlockpt
    // are required before opening the slave device.
    *fdm = posix_openpt(O_RDWR | O_NOCTTY);
    if (*fdm == -1 || grantpt(*fdm) == -1 || unlockpt(*fdm) == -1)
        goto FAILED_SETUP;

    // ptsname returns a string that must be copied, as it is overwritten
    // on subsequent calls.
    const char *tmp = ptsname(*fdm);
    if (tmp == NULL) 
        goto FAILED_SETUP;
    
    size_t tmp_len = strlen(tmp) + 1;
    *name = malloc(tmp_len * sizeof(char));
    if (*name == NULL)
        goto FAILED_SETUP;

    memcpy(*name, tmp, tmp_len);
    
    *fds = open(*name, O_RDWR | O_NOCTTY);
    if (*fds == -1)
        goto FAILED_SETUP;
    
    // ensure both slave and master close after program finishes    
    fcntl(*fdm, F_SETFD, FD_CLOEXEC);
    fcntl(*fds, F_SETFD, FD_CLOEXEC);    

    // set master as non-blocking (for get_process_output functions)
    fcntl(*fdm, F_SETFL, O_NONBLOCK);

    // Set raw mode
    struct termios tty;
    tcgetattr(*fds, &tty);
    cfmakeraw(&tty);
    tcsetattr(*fds, TCSANOW, &tty);

    return 0;

FAILED_SETUP:
    if (*fdm != -1) close(*fdm);
    if (*fds != -1) close(*fds);
    if (*name != NULL) free(*name);
    
    *fdm = -1;
    *fds = -1;
    *name = NULL;
    return -1;
}

struct process* create_process(char *const command[], const char *path) {
    // Unix PTYs are bi-directional communication streams.  Typically, a terminal will
    // combine stdout and stderr and display them in the same output.  We want to
    // keep the outputs separate at this level so master_pty_er is created just to carry
    // the stderr stream.
    //
    // There is a potential bug here, if the process tries to set terminal attributes (like
    // with stty), these updates won't be propogated across both terminals.

    int master_pty_io, slave_pts_io, master_pty_er, slave_pts_er;
    char *pts_io_name, *pts_er_name;
    int ret;

    ret = open_pty(&master_pty_io, &slave_pts_io, &pts_io_name);
    if (ret == -1)
        goto FAILED_SETUP;

    ret = open_pty(&master_pty_er, &slave_pts_er, &pts_er_name);
    if (ret == -1)
        goto FAILED_SETUP;

    // START CHILD PROCESS AND RETURN ITS PID
    pid_t pid = fork();

    if (pid == -1) {
        goto FAILED_SETUP;
    } else if (pid != 0) {
        close(slave_pts_io);
        close(slave_pts_er);
        // parent process, return process structure.
        struct process *p =  allocate_process(master_pty_io, master_pty_er,
                                              pts_io_name, pts_er_name, pid);
        
        // allocate_process copies the strings it is passed, open_pty mallocs strings
        // so we need to free them here before we exit.
        free(pts_io_name); 
        free(pts_er_name);
        return p;
    }
    
    // VVV CHILD PROCESS VVV
    setsid();
    
    // we don't need these in the child process.
    free(pts_io_name);
    free(pts_er_name);
    close(master_pty_io);
    close(master_pty_er);

    dup2(slave_pts_io, STDIN_FILENO);
    dup2(slave_pts_io, STDOUT_FILENO);
    dup2(slave_pts_er, STDERR_FILENO);

    close(slave_pts_io);
    close(slave_pts_er);

    if (path != NULL) chdir(path);
    
    // run command, the current fork process will switch to
    // the command.
    execvp(command[0], command);

    // if execution reaches here, there was a problem starting
    // the program.  execvp does not return on success.
    int error_status = errno;
    if (error_status == ENOENT) {
        char str[128];
        sprintf(str, "%s: command not found", command[0]);
        write(STDIN_FILENO, str, strlen(str));
    } else {
        char *str = strerror(error_status);
        write(STDIN_FILENO, str, strlen(str));
    }
    my_exit(error_status);

  // ERROR HANDLING
FAILED_SETUP:
    // we can assume at this point that any FD that is not -1 needs closed.
    if (master_pty_io != -1) close(master_pty_io);
    if (master_pty_er != -1) close(master_pty_er);
    if (slave_pts_io != -1) close(slave_pts_io);
    if (slave_pts_er != -1) close(slave_pts_er);
    
    // we can assume that any name pointer that is not NULL needs free.
    if (pts_io_name != NULL) free(pts_io_name);
    if (pts_er_name != NULL) free(pts_er_name);

    return NULL;
}

int process_pid(struct process *process) {
    return process->pid;
}

void process_write_string(struct process *process, const char *string) {
    write(process->fd_io, string, strlen(string));
}

void process_write(struct process *process, const char *buf, size_t n) {
    write(process->fd_io, buf, n);
}

// reads all data available in fd (should be non-blocking) into str,
// returns number of bytes read on success, -1 on error.
int str_read_fd(struct str *str, int fd) {
    int total_read = 0;
    while (true) {
        // resize buffer if it is too small (include space for null terminator)
        if (str->cap - str->len <= 1) {
            char *new_ptr = realloc(str->buf, 2*str->cap);
            if (new_ptr == NULL)
              return -1;
            str->buf = new_ptr;
            str->cap *= 2;
        }

        // read as much data from fd as possible.
        int n = read(fd, str->buf + str->len, str->cap - str->len);
        
        if (total_read == 0 && n == -1) {
            return -1; // an error occured on first read
        } else if (n <= 0) {
            str->buf[str->len] = '\0';
            return total_read;
        }
        
        total_read += n;
        str->len += n;
    }

    return -1;  // control flow shouldn't reach here.
}

char* _process_receive_fd(struct str *s, int fd) {
    int n = str_read_fd(s, fd);
    if (n == -1)
        return NULL;

    char *ret = malloc((s->len + 1) * sizeof(char));
    if (ret == NULL)
        return NULL;

    memcpy(ret, s->buf, s->len);
    ret[s->len] = '\0';

    s->len = 0;
    return ret;
}

char* process_receive_stdout(struct process *p) {
    return _process_receive_fd(&p->stdout, p->fd_io);
}

char* process_receive_stderr(struct process *p) {
    return _process_receive_fd(&p->stderr, p->fd_er);
}

char* process_receive_output(struct process *process) {
    char *stdout = process_receive_stdout(process);
    char *stderr = process_receive_stderr(process);

    if (stdout == NULL && stderr == NULL)
        return NULL;

    if (stdout != NULL && stderr == NULL)
        return stdout;

    if (stdout == NULL && stderr != NULL)
        return stderr;

    size_t o_len = strlen(stdout);
    size_t e_len = strlen(stderr);
    size_t length = o_len + e_len + 1;
    char *ret = malloc(length * sizeof(char));
    if (ret == NULL)
        return NULL;

    memcpy(ret, stdout, o_len);
    memcpy(ret + o_len, stderr, e_len);

    ret[length] = '\0';

    free(stdout);
    free(stderr);

    return ret;
}

int process_alive_p(struct process *process)
{
    return kill(process->pid, 0) == 0;
}
