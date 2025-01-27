CC := gcc
CFLAGS := -std=c99 -Wall -Wextra -g -O0
LDFLAGS :=
APP_NAME := receval2

SRCS := receval.c parse.c
OBJS = $(patsubst %.c, %.o, $(SRCS))

all: $(OBJS)
	$(CC) $(LDFLAGS) $^ -o $(APP_NAME)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm $(APP_NAME) *.o

.PHONY: all clean
