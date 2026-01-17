#ifndef _ASYNC_PROCESS_H_
#define _ASYNC_PROCESS_H_

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define _GNU_SOURCE
#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <termios.h>

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

struct str {
    char* buf;
    size_t len;
    size_t cap;
};

int init_str(struct str *str);
void del_str(struct str *str);
int str_read_fd(struct str *str, int fd);

struct process {
    struct str stdout;
    struct str stderr;
    struct str both;

    int fd_io;
    int fd_er;
    char *pts_io_name;
    char *pts_er_name;
    pid_t pid;
};

struct process* create_process(char *const command[], const char *path);
void delete_process(struct process *process);
int process_pid(struct process *process);

/** Sends n bytes to process.

returns the number of bytes written, or -1 indicating an error occurred.  An
error will typically occur when the operating system cannot send all n bytes
because the PTY buffer is full.  The process will have to read the buffer in
to make space for more data to be written.

These functions read from the process STDOUT and STDERR buffers to keep
the process from being blocked.  The results are buffered and will be returned
on the next call to a process_receive function.  If these functions are used
in a separate thread from the process_receive functions, a race condition may
occur when this function is reading from STDOUT/STDERR (possibly realloc'ing)
while `process-receive_*` is reading from the same buffer.
*/
ssize_t process_write(struct process *process, const char* buf, size_t n);
ssize_t process_write_string(struct process *process, const char *string);

/** Sends n bytes to process.

returns the number of bytes written, or -1 indicating an error occurred.  An
error will typically occur when the operating system cannot send all n bytes
because the PTY buffer is full.  The process will have to read the buffer in
to make space for more data to be written.

Doesn't read devices STDOUT/STDERR file descriptors.  If `process_receive*
functions are not called regularly, the internal PTY buffers may fill and
prevent the attached process from continuing to run.  `process_write` and 
`process_write_string` prevent this from happening, but their usage requires
other considerations.
*/
ssize_t process_write_noread(struct process *process, const char* buf, size_t n);
ssize_t process_write_string_noread(struct process *process, const char *string);

/** Return Process STDOUT.
Returns pointer to a buffer containing data returned by process STDOUT buffer.
this buffer will be overwritten by subsequent calls to this function; if 
this output is meant to be kept, it should be copied out.
*/
const char* process_receive_stdout(struct process *process);

/** Return Process STDERR.
Returns pointer to a buffer containing data returned by process STDERR buffer.
this buffer will be overwritten by subsequent calls to this function; if 
this output is meant to be kept, it should be copied out.
*/
const char* process_receive_stderr(struct process *process);

/** Receive Process STDOUT and STDERR (one after another).
Returns pointer to a buffer containing data returned by process STDERR and 
STDOUT buffer. this buffer will be overwritten by subsequent calls to this
 function; if  this output is meant to be kept, it should be copied out.
*/
const char* process_receive_output(struct process *process);

int process_alive_p(struct process *process);

#endif
