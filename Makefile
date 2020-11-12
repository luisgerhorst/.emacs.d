.PHONY: all
all: lib/LanguageTool-5.0/languagetool-commandline.jar

.PHONY: lib/LanguageTool-5.0/languagetool-commandline.jar
lib/LanguageTool-5.0/languagetool-commandline.jar:
	mkdir -p $(dir $@) \
	&& wget https://languagetool.org/download/LanguageTool-5.0.zip -O $@ -T 10 \
	&& rm -rfd lib/LanguageTool-5.0 \
	&& unzip $< -d lib \
	&& touch $@ \
	|| true
