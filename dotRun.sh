#!/bin/bash

clear
echo ".run maker by SuperLuFi"

select opsi in "Membuat installer.run" "Menjalankan Installer" Tool Contact Exit
do
case $opsi in

	"Membuat installer.run")
	cd Run
	echo Melakukan proses pembuatan unstaller
	echo Pilih arsitektur CPU [x86 atau x64]

	select pilih_jenis in "x32 atau x86" x64 Kembali
	do
	case $pilih_jenis in

		"x32 atau x86")
		cp ../.SRACx32 ./
		makeself --gzip --current . SRACx32.run "SRAC Installer untuk Debian/Ubuntu Linux | Fisika UIN Bandung" ./install32.sh
		exit 0
		;;

		x64)
		cp ../.SRACx64 ./
		makeself --gzip --current . SRACx64.run "SRAC Installer untuk Debian/Ubuntu Linux | Fisika UIN Bandung" ./install64.sh
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

	"Menjalankan Installer")
	echo Menjalankan installer ...
	echo Memindahkan file.run
	cd Lab && mv *.run ../Run  #Masuk folder Lab dan move file.run ke folder Run
	echo File.run sudah dipindahkan
	echo Memulai file.run
	cd ../Run && ./*.run       #Masuk folder Run dan eksekusi file.run
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