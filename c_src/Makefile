TAR		?= tar
GUNZIP		?= gunzip
HAMSTER_VER 	= 1.1.7

all: dist

dist: hamsterdb-$(HAMSTER_VER).tar.gz libhamsterdb.a
	@rm -rf hamsterdb-$(HAMSTER_VER)

hamsterdb-$(HAMSTER_VER).tar.gz:
	wget -nv http://hamsterdb.com/public/dl/hamsterdb-1.1.6.tar.gz

libhamsterdb.a:
	$(GUNZIP) -c hamsterdb-$(HAMSTER_VER).tar.gz | $(TAR) xf -
	cd hamsterdb-$(HAMSTER_VER);CFLAGS="-fPIC" ./configure --disable-server --disable-remote --disable-encryption
	make -C hamsterdb-$(HAMSTER_VER)
	cp hamsterdb-$(HAMSTER_VER)/src/.libs/libhamsterdb.a .
	mkdir include;cp -R hamsterdb-$(HAMSTER_VER)/include/ham include
clean:
	rm -rf hamsterdb-$(HAMSTER_VER) include
	rm -f libhamsterdb.a