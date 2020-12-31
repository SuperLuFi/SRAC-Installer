    #Menginstall aplikasi yang dibutuhkan untuk running SRAC
    #SRAC tidak akan berjalan tanpa aplikasi-aplikasi ini
    #Dan instalasi wajib menggunakan koneksi internet
    echo
    echo "Menginstal aplikasi yang diperlukan..."
    echo "Update daftar paket, masukan sandi root ..."
    sudo apt-get update -y
    clear
    echo "Instalasi compiler C"
    sudo apt-get install build-essential gcc -y &&
    echo "Instalasi compiler Fortran"
    sudo apt-get install fort77 f2c -y &&
    echo "Instalasi code editor"
    sudo apt-get install atom csh -y &&
    echo "Instalasi GUI Tools"
    sudo apt-get install zenity dialog -y
    clear
    echo "Instalasi offline dependecies ..."
    cd Preinstall_pack_64bit && ./install.sh
    cd ..
    clear
    echo "Persiapan selesai"