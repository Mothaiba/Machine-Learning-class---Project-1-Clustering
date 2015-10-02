trySomeK <- function(img, nK = 30, seed = 123){
  ssError = array(nK);
  for (i in 1 : nK){
    set.seed(seed);
    ssError[i] = sum(kmeans(img, centers = i, iter.max = 20)$withinss)
  }
  plot(1 : nK, ssError, type = "b", xlab = "The Number of Clusters", ylab = "Sum of Square Error");
  dev.copy(jpeg, 'the_number_of_cluster_and_their_error.jpeg');
  dev.off();
}

simplizeByDivide <- function(img, rmin, rmax, cmin, cmax, nrec){
  
  sr = mean(img[rmin : rmax, cmin : cmax, 1]); 
  sg = mean(img[rmin : rmax, cmin : cmax, 2]);   
  sb = mean(img[rmin : rmax, cmin : cmax, 3]);  
                                
  re = c(sr, sg, sb);
  if(nrec < 3){
    rmid = (rmin + rmax) / 2;
    cmid = (cmin + cmax) / 2;
    re = c(re, simplizeByDivide(img, rmin, rmid, cmin, cmid, nrec + 1),
               simplizeByDivide(img, rmin, rmid, cmid + 1, cmax, nrec + 1),
               simplizeByDivide(img, rmid + 1, rmax, cmin, cmid, nrec + 1),
               simplizeByDivide(img, rmid + 1, rmax, cmid + 1, cmax, nrec + 1)); 
  }
  
  return (re);
                                
}

simplizeByHistogram <- function(img, rmin, rmax, nrec){
  
  if(nrec == 1)
    img = floor(floor(img * 255) / 32) + 1;
  
  histo = array(0 * 1 : 24, c(3, 8));
  
  for(i in rmin : rmax){
    for(j in 1 : 64){
      for(k in 1 : 3){
        histo[k, img[i, j, k]] = histo[k, img[i, j, k]] + 1;
      }
    }
  }
  
  histo = histo / (rmax - rmin + 1) / 64;
  histo = matrix(histo, nrow = 1);
  
  if(nrec < 2){
    rmid = (rmin + rmax) / 2;
    histo = c(histo, simplizeByHistogram(img, rmin, rmid, nrec + 1),
              simplizeByHistogram(img, rmin, rmid, nrec + 1),
              simplizeByHistogram(img, rmid + 1, rmax, nrec + 1),
              simplizeByHistogram(img, rmid + 1, rmax, nrec + 1)); 
  }
  
  return (histo);
  
}

simplizeByDomination <- function(img){
  img = floor(floor(img * 255) / 16);
  cnt = c(0, 0, 0, 0, 0, 0, 0);
  
  nr = nrow(img[,,1]);
  nc = ncol(img[,,1]);
  for(i in 1 : nr){
    for(j in 1 : nc){
      sr = img[i, j, 1];
      sg = img[i, j, 2];
      sb = img[i, j, 3];
      
      if(sr > sg && sr > sb) {cnt[1] = cnt[1] + 1}
      else if(sg > sr && sg > sb) {cnt[2] = cnt[2] + 1}
      else if(sb > sr && sb > sg) {cnt[3] = cnt[3] + 1}
      else if(sr == sg && sr > sb) {cnt[4] = cnt[4] + 1}
      else if(sr == sb && sr > sg) {cnt[5] = cnt[5] + 1}
      else if(sg == sb && sg > sr) {cnt[6] = cnt[6] + 1}
      else {cnt[7] = cnt[7] + 1}
    }  
  }
  cnt = cnt / sum(cnt);
  return (cnt);
}

toHTML <- function(imageFiles, clus, nclus, htmlName){
  
  iprefix = '<img src = "images/';
  isuffix = '" />';
  newline = '<br> </br>';
  
  
  write("", file = htmlName);
  write('<!DOCTYPE html>', file = htmlName, sep = "\n", append = T);
  write('<html> <body>', file = htmlName, sep = "\n", append = T);
  
  
   write( '<div align = "center">
           <div> <h2>Phung Minh Tung</h3> </div>
           <div> <h3>CS0801 - Machine Learning - Project 1: Clustering</h3> </div>
           </div>', file = htmlName, sep = "\n", append = T);
  write('<div>', file = htmlName, sep = "\n", append = T);
  write('<p>A Plot of how Sum of Square Error varies when we change The number of Clusters:</p>', file = htmlName, sep = "\n", append = T);
  write(newline, file = htmlName, sep = "\n", append = T);
  write(paste('<img src = "', 'the_number_of_cluster_and_their_error.jpeg', isuffix, sep = ''), file = htmlName, sep = "\n", append = T);
  write(newline, file = htmlName, sep = "\n", append = T);
  write(paste('To balance both The number of Cluster and The Error
        so that each of them is not so large, I decide to choose k equals', 
        toString(nclus)), file = htmlName, sep = "\n", append = T);
  write(newline, file = htmlName, sep = "\n", append = T);
  
  nimages = length(clus);
  
  for (i in 1 : nclus){
    write(newline, file = htmlName, sep = "\n", append = T);
    write(paste('Cluster ', toString(i), ':', sep = ''), file = htmlName, sep = "\n", append = T);
    write(newline, file = htmlName, sep = "\n", append = T);
    dem = 0;
    for (j in 1 : nimages){
      if(clus[j] == i){
        if(dem %% 15 == 0){
          write(newline, file = htmlName, append = T, sep = '');
        }
        write(paste(iprefix, basename(imageFiles[j]), isuffix, sep = ""), file = htmlName, append = T, sep = " ");
        dem = dem + 1;
      }
    }
  }
  write('</div>', file = htmlName, sep = "\n", append = T);
  write('</body> </html>', file = htmlName, sep = "\n", append = T);
  
  
}


library(jpeg);
imageFiles = list.files('C:/Users/Tung/Documents/images', full.names = T);

df = data.frame();
for (each in imageFiles){
  img = readJPEG(each);
  df = rbind(df, c(simplizeByHistogram(img, 1, 64, 1) * 0.3,
                   simplizeByDomination(img) * 0.59,
                   simplizeByDivide(img, 1, 64, 1, 64, 1) * 0.11) );
}

trySomeK(df);
nclus = 20;
model = kmeans(df, centers = nclus, iter.max = 20);
clus = model$cluster;
toHTML(imageFiles, clus, nclus, 'TungPM_project1_Clustering.html');


