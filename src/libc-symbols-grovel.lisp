(in-package async-process)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(include "fcntl.h")
(constant (+o-rdwr+ "O_RDWR"))
(constant (+o-noctty+ "O_NOCTTY"))
(constant (+o_nonblock+ "O_NONBLOCK"))

(constant (+f-setfd+ "F_SETFD"))
(constant (+f-setfl+ "F_SETFL"))

(constant (+fd-cloexec+ "FD_CLOEXEC"))

(constant (+tcsanow+ "TCSANOW"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(include "unistd.h")
(constant (+stdin-fileno+ "STDIN_FILENO"))
(constant (+stdout-fileno+ "STDOUT_FILENO"))
(constant (+stderr-fileno+ "STDERR_FILENO"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(include "errno.h")
(constant (+enoent+ "ENOENT"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(include "termios.h")
(cstruct termios "struct termios")
