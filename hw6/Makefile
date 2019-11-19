all:
	make check

check:
	kotlinc everyNth.kt -include-runtime -d everyNth.jar
	java -jar everyNth.jar

clean:
	rm -f -r *.jar *.class META-INF *.tar.gz

dist:
	tar -czf hw6.tar.gz Makefile everyNth.kt