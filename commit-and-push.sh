#!/bin/bash

read -p "Masukan commit label :  " commit
git add .
git status
read -p "Tekan enter untuk melanjutkan :  " random
git commit -m "$commit"
git push -u origin main
