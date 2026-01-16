#include "async-process.h"

int main() {
    char *cmd[] = {"tee", "ima-cool-file", NULL};
    struct process *p = create_process(cmd, NULL);

    struct str s;
    init_str(&s);

    #define TEST_INPUT_SIZE 50000
    char test_input[TEST_INPUT_SIZE];

    for (size_t i = 0; i < TEST_INPUT_SIZE; i++) {
        test_input[i] = '0' + (i % 10);
    }
    
    size_t n = 0;
    while (n != TEST_INPUT_SIZE) {
        ssize_t bytes = process_write(p, test_input+n, TEST_INPUT_SIZE-n);
        if (bytes > 0) {
            printf("writing %d/%d bytes...\n", bytes, n);
            n += bytes;        
        } else {
            printf("error: %s\n", strerror(errno));
        }
            
    }
    
    printf("I just attempted to write %d.\nI wrote %d bytes.\n", TEST_INPUT_SIZE, n);
    
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
