CC := gcc
CFLAGS := -std=c99 -Wall -Wextra -g -O0
LDFLAGS :=
APP_NAME := receval

SRCS := receval.c parse.c tokenizer.c
HEADERS := tokenizer.h common.h da.h parse.h
OBJDIR := obj
OBJS = $(patsubst %.c, $(OBJDIR)/%.o, $(SRCS))

all: $(OBJS)
	$(CC) $(LDFLAGS) $^ -o $(APP_NAME)

$(OBJDIR)/%.o: %.c $(HEADERS)
	mkdir -p $(OBJDIR)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm $(APP_NAME) *.o

.PHONY: all clean
