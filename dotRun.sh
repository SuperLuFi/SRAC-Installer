#!/bin/bash

clear
echo ".run maker by SuperLuFi"

select opsi in "Membuat installer.run" "Menjalankan Installer" Tool Contact Exit
do
case $opsi in

	"Membuat installer.run")
	cd Run
	echo Melakukan proses pembuatan installer
	echo
	echo Tentukan versi aplikasi
	echo Pilih arsitektur CPU [x86 atau x64]

	select pilih_jenis in "x32 atau x86" x64 Kembali
	do
	case $pilih_jenis in

		"x32 atau x86")
		cp -r ../.SRACx32 ./
		cp ../install32.sh ./
		makeself --gzip --current . SRACx32_DebianUbuntu.run "SRAC Installer untuk Debian/Ubuntu Linux | Fisika UIN Bandung" ./install32.sh
		rm -rf *
		mv -f ./*.run ../Bin
		exit 0
		;;

		x64)
		cp -r ../.SRACx64 ./
		cp ../install64.sh ./
		makeself --gzip --current . SRACx64_DebianUbuntu.run "SRAC Installer untuk Debian/Ubuntu Linux | Fisika UIN Bandung" ./install64.sh
		rm -rf *
		mv -f ./*.run ../Bin
		exit 0
		;;

		Kembali)
		;;

		*)
		echo Masukan angka yang sesuai.
		;;
	esac
	done
	cd ..
	;;

	"Tool")
	echo Instalasi Tool
	sudo apt update && sudo apt install makeself
	;;

	"Contact")
	echo Contact me on telegram https://t.me/mango_yakult
	;;

	"Exit")
	clear
	exit 0
	;;

	*)
	echo Masukan angka yang sesuai 
	;;

esac
done