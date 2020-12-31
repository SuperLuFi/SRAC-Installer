#     !/bin/bash

################################
#                              #
#     Created by :             #
#        Syaiful Bahri Al Haq  #
#        Fisika Nuklir Reaktor #
#        UIN SGD Bandung       #
################################

echo "Terima kasih, telah menggunakan installer ini!"
sudo dpkg -i gcc-3.4-base_3.4.6-6ubuntu3_i386.deb cpp-3.4_3.4.6-6ubuntu3_i386.deb gcc-3.4_3.4.6-6ubuntu3_i386.deb libg2c0_3.4.6-6ubuntu3_i386.deb libg2c0-dev_3.4.6-6ubuntu3_i386.deb g77-3.4_3.4.6-6ubuntu3_i386.deb
echo "Selesai. Memasang g77 fortran compiler pada sistem."
sudo ln -s /usr/bin/g77-3.4 /usr/bin/g77
echo "Pemasangan fortran compiler selesai."
echo "Instalasi selesai. Harap baca laporan di atas bila terjadi error!"