.PHONY : all

all: .stignore

.stignore :
	cp .make/stignore-template .stignore
