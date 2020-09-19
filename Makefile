.PHONY: all
all: lib/LanguageTool-5.0/languagetool-commandline.jar

lib/LanguageTool-5.0.zip:
	mkdir -p $(dir $@) \
	&& wget https://languagetool.org/download/LanguageTool-5.0.zip -O $@

lib/LanguageTool-5.0/languagetool-commandline.jar: lib/LanguageTool-5.0.zip
	rm -rfd lib/LanguageTool-5.0 \
	&& unzip $< -d lib \
	&& touch $@
