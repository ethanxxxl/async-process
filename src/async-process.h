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

    int fd_io;
    int fd_er;
    char *pts_io_name;
    char *pts_er_name;
    pid_t pid;
};

struct process* create_process(char *const command[], const char *path);
void delete_process(struct process *process);
int process_pid(struct process *process);

void process_write(struct process *process, const char* buf, size_t n);
void process_write_string(struct process *process, const char *string);

/** receive process stdout.  MUST FREE RETURNED PONTER */
char* process_receive_stdout(struct process *process);
/** receive process stderr.  MUST FREE RETURNED PONTER */
char* process_receive_stderr(struct process *process);
/** receive process stdout and stderr (one after another).  
MUST FREE RETURNED PONTER */
char* process_receive_output(struct process *process);
int process_alive_p(struct process *process);

#endif
