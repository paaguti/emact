# EmACT RPM spec file for emact 3.00.0
# @(#)emact.spec.in	(c) C. Jullien 2008/03/17"

Summary: EmACT is a GPL'ed GNU Emacs clone.
Name: emact
Version: 3.00.0
Release: 0
Copyright: Eligis (c) 1986-2018
Vendor: Eligis
Packager: Christian Jullien <jullien@eligis.com>
Group: Editor
Source: emact-3.00.0.tar.gz
URL: http://www.eligis.com/emacs
#BuildRoot: /home/jullien

%description
EmACT is a small GNU Emacs clone that run almost on any OS.

%changelog
* Mon Jun 18 2001 <jullien@eligis.com>
  * see README file
%prep
%setup
%build
./configure
# ./configure --prefix=$RPM_BUILD_ROOT
make
%install
rm -rf /usr/local/emact
make install
%clean
make clean
%files
/usr/local/bin/emact
%post
	echo Emact has been installed in /usr/local/emact
%postun
	echo Emact has been removed from /usr/local/emact
