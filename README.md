### Perhatian! Tidak untuk dipublikasikan. Hak cipta milik JAEA.

## Pengenalan 

Installer ini saya buat untuk mempermudah proses instalasi SRAC pada linux terbaru. Linux dengan repositori terbarukan, akan cenderung memiliki GCC ataupun fortran compiler versi terbaru. Versi terbaru ini sangat sulit digunakan untuk melakukan proses kompilasi source code SRAC, baik versi 2002 maupun 2007. Melakukan downgrade GCC akan sangat berbahaya, karna banyak aplikasi linux yang bergantung pada GCC versi terbaru ini. Selain itu, GCC versi lama sudah tidak tersedia di repositori debian/ubuntu terbaru.

## Install.sh

File awalan untuk membuat .run installer dengan makeself.
Buat folder kosong. Dan letakan install.sh serta folder .SRACx64.
Installer makefile ada pada komen sourcecodetxt, silahkan edit.

## Persiapan.sh

Install dependencies.

## InstallerSRAC.sh

Installer inti yang melakukan proses instalasi SRAC berdasarkan versi yang dipilih.

# SourceCodetxt

Merupakan file installer yang menyatukan semua tools untuk digunakan sebagai installer yang sebenarnya.

## Instalasi

Silahkan buka homepage dari SRAC-Installer http://superlufi.github.io/#srac-installer. If you are looking for english translation, go to this page https://superlufi-github-io.translate.goog/?_x_tr_sl=id&_x_tr_tl=en&_x_tr_hl=id&_x_tr_pto=wapp#srac-installer 
