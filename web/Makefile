##
## EPITECH PROJECT, 2024
## ImageCompressor
## File description:
## Makefile for stack
##

NAME 		= 	imageCompressor


BINARY_PATH 	:=	$(shell stack path --local-install-root)


all	:
		stack build
		cp $(BINARY_PATH)/bin/ImageCompressor-exe ./$(NAME)

clean	:
		stack clean

fclean	:	clean
		rm -f $(NAME)

re	:	fclean all

.PHONY	:	all clean fclean re
