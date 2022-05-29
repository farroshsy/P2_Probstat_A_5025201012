# P2_Probstat_A_5025201012

Praktikum Probabilitas dan Statistik 2022 Modul 2 (Distribusi Probabilitas)

## Generated By:
| Nama          | NRP                                                      |  Kelas                                                     | 
| ------------- | -------------------------------------------------------- | --------------------------------------------------------   |
| Farros Hilmi Syafei | 5025201012 | Probabilitas dan Statistik A |

## Dosen: Dr. Ahmad Saikhu, S.Si., MT.

<br />

## Soal 1
> Seorang peneliti melakukan penelitian mengenai pengaruh aktivitas 𝐴 terhadap kadar saturasi oksigen pada manusia. Peneliti tersebut mengambil sampel sebanyak 9 responden. Pertama, sebelum melakukan aktivitas 𝐴, peneliti mencatat kadar saturasi oksigen dari 9 responden tersebut. Kemudian, 9 responden tersebut diminta melakukan aktivitas 𝐴. Setelah 15 menit, peneliti tersebut mencatat kembali kadar saturasi oksigen dari 9 responden tersebut. Berikut data dari 9 responden mengenai kadar saturasi oksigen sebelum dan sesudah melakukan aktivitas 𝐴

![soal 1](https://user-images.githubusercontent.com/70510279/170801862-fb8feada-e470-4bdd-90c8-2b1b050563ca.jpg)

Berdasarkan data pada tabel diatas, diketahui kadar saturasi oksigen dari responden ke-3 ketika belum melakukan aktivitas 𝐴 sebanyak 67, dan setelah melakukan aktivitas 𝐴 sebanyak 70.

  - A. Carilah Standar Deviasi dari data selisih pasangan pengamatan tabel diatas

    Langkah pertama penyelesaian adalah memasukkan semua data yang ada pada tabel pada sebuah variabel sebagai berikut

        ```
         # Poin 1a
          before <- c(78, 75, 67, 77, 70, 72, 28, 74, 77)
          after <- c(100, 95, 70, 90, 90, 90, 89, 90, 100)

        ```  

    Setelah dimasukkan semua datanya, maka dilakukan cek data menggunakan `data.frame` yaitu

        ```
          my_data <- data.frame(
              group = rep(c("before", "after"), each = 9),
              saturation = c(before, after)
          )
        ```

    Setelah itu dilihat hasil framenya sebagai berikut:

        ```R
          print(my_data)
        ```

    <img width="435" alt="image" src="https://user-images.githubusercontent.com/86004023/170858094-792f1a04-8114-4add-80cf-c51cdc4fcd86.png">

    Selanjutnya mencari standar deviasinya. Standar deviasi sebelum dan sesudah aktivitas adalah
    
      ```
          # Standar Devisiasi before activity
          SD_before <- sd(before)
          SD_before
          
           # Standar Devisiasi after activity
           SD_after <-sd(after)
           SD_after
       ```         
     Maka hasilnya : 

     <img width="347" alt="image" src="https://user-images.githubusercontent.com/86004023/170858149-b2e4b403-f4a1-4515-97bd-e990edcbd699.png">

</br>

   - B. carilah nilai t (p-value)
      Untuk mencari nilai t (p-value) dapat menggunakan fungsi `t.test` yaitu sebagai berikut
        ```
         # Menggunakan t-test
         t.test(before, after, alternative = "greater", var.equal = FALSE)
       ```    
       Sehingga Hasilnya sebagai berikut:
       
      <img width="616" alt="image" src="https://user-images.githubusercontent.com/86004023/170858480-4e5c7d24-b5be-4ad1-8c0f-554450bbb4ca.png">
  
  
   - C. Tentukanlah Apakah Terdapat Pengaruh yang Signifikan Secara Statistika dalam Hal Kadar Saturasi Oksigen, Sebelum dan Sesudah Melakukan Aktivitas 𝐴  jika Diketahui Tingkat Signifikansi 𝛼 = 5% serta H0 : “tidak ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴”
       Langkah pertama yaitu melihat hasil komparasi dua variabel berikut
        ```
         var.test(before, after)
       ```    
       Sehingga Hasilnya sebagai berikut:
       
       <img width="593" alt="image" src="https://user-images.githubusercontent.com/86004023/170858996-31f369f7-66ad-4279-9b12-2562a1f0e027.png">
       
       Selanjutnya, untuk melihat pengaruh jika tingkat signifikasi 5% dan tidak ada pengaruh yang signifikan secara statistika, maka adalah sebagai berikut
       ```
         t.test(before, after, mu = 0, alternative = "two.sided", var.equal = TRUE)
       ```  
      Sehingga Hasilnya sebagai berikut:

      <img width="692" alt="image" src="https://user-images.githubusercontent.com/86004023/170859040-a1c6aaaa-0fe0-4834-8778-eedcf4530034.png">

       
