#include "async-process.h"

int main() {
    char *cmd[] = {"tee", "ima-cool-file", NULL};
    struct process *p = create_process(cmd, NULL);

    struct str s;
    init_str(&s);

    while (true) {
        int n = str_read_fd(&s, STDIN_FILENO);
        if (n > 0) {
            process_write(p, s.buf, n);
            s.len = 0;
            if (strcmp(s.buf, "exit\n") == 0)
                break;
        }

        char *out = NULL;
        char *err = NULL;

        out = process_receive_stdout(p);
        err = process_receive_stderr(p);
        
        if (out != NULL) {
            printf("%s", out);
            free(out);
        }

        if (err != NULL) {
            printf("\033[31m%s\033[0m", err);
            free(err);
        }        
    }

    delete_process(p);
    del_str(&s);
    return 0;
}
