NASM	= nasm
RM		= rm
CC		= gcc
OBJECTS	= main.o
TARGET	= main

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $? -o $@

%.o: %.S
	$(NASM) -f elf64 $? -o $@

clean:
	$(RM) -rf $(OBJECTS)

fclean: clean
	$(RM) -rf $(TARGET)

re: fclean all

.PHONY: all re clean fclean