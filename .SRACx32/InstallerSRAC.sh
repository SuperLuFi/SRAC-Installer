    kembali=1
    while [ $kembali == '1' ]; #Kembali 1 untuk memilih ulang versi SRAC
    do
      echo "Saya telah menyediakan 2 versi SRAC"
      echo
      echo "Silahkan pilih Versi SRAC yang akan diinstall:"
      echo "1. SRAC Versi 2002"
      echo "2. SRAC Versi 2007"
      echo
      echo "Masukan angka 1 atau 2"
      read Versi
      #Versi=$(zenity --list --title="Silahkan pilih Versi SRAC yang akan di install" --column=" No." --column="Versi SRAC " "1" "SRAC Versi 2002" "2" "SRAC Versi 2007")
      export Versi
      if [ $Versi == 1 ]; then # SRAC 2002
        echo "Anda memilih SRAC 2002"
        echo "Memulai instalasi SRAC 2002..."
        cd S2002 && cp -r SRAC $HOME
        cd ..
        cp -r SRACLIB-JDL32 $HOME
        cp -r SRACLIB-JDL33 $HOME
        echo "Instalasi selesai"
        echo
        echo "Installer credit by Syaiful Bahri Al Haq"
        echo "Contact me on telegram https://t.me/mango_yakult"
        
        #Pengambilan sample output S2002
        echo
        echo "Memeriksa aplikasi ..."
        cd ~/SRAC/smpl/shr/
        waktu=$(date +'%b%d.%H.%M.%S')
        ./Test.sh
        cd .. && cd outp
        kinerja=$( tail -n 1 "Test.SFT06.$waktu" | cut -c '3' )
        
        #Melakukan Pemeriksaan SRAC 2002
        if [[ $kinerja == "=" ]];
        then
          echo
          echo "SRAC telah berfungsi dengan baik";
        else
          echo
          echo "Instalasi telah gagal, apakah anda memilih arsitektur yang sesuai?"
          echo "Gunakan SRACx64 untuk simtem 64bit dan SRACx32 untuk sistem 32bit."
        fi
        
        kembali='t';
        
      elif [ $Versi == 2 ]; then # SRAC 2007
        echo "Anda memilih SRAC 2007"
        echo "Memulai instalasi SRAC 2007..."
        cd S2007 && cp -r SRAC $HOME
        cd ..
        cp -r SRACLIB-JDL32 $HOME
        cp -r SRACLIB-JDL33 $HOME
        echo "Instalasi selesai"
        echo
        echo "Installer credit by Syaiful Bahri Al Haq"
        echo "Contact me on telegram https://t.me/mango_yakult"
                    
        #Pengambilan sample output S2007
        echo
        echo "Memeriksa aplikasi ..."
        cd ~/SRAC/smpl/shr/
        waktu=$(date +'%Y.%m.%d.%H.%M.%S')
        ./Test.sh
        cd .. && cd outp
        kinerja=$( tail -n 1 "Test.SFT06.$waktu" | cut -c '3' )
        
        #Melakukan Pemeriksaan SRAC 2007
        if [[ $kinerja == "=" ]];
        then
          echo
          echo "SRAC telah berfungsi dengan baik";
        else
          echo
          echo "Instalasi telah gagal, apakah anda memilih arsitektur yang sesuai?"
          echo "Gunakan SRACx64 untuk simtem 64bit dan SRACx32 untuk sistem 32bit."
        fi
        
        kembali='t';
                   
      else
        echo
        echo " Maaf, Masukan Angka [1-2] "
        read -p " Tekan Enter Untuk Memilih Kembali"
        kembali='1';
      fi

    done