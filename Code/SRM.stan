
data{
 int N;
 int P;
 int M;

 array[2] real Q;

 array[N,N] int Giving;
 array[N,N] int Taking;
 array[N,N] int Reducing;
 array[N,N] int Transfer;
 array[N,N] int Friends;

 matrix[N,N] SameEthnicity;
 matrix[N,N] SameSex;
 matrix[N,N] Other;
 matrix[N,N] Relatedness;
 matrix[N,N] Marriage;

 vector[N] Age;
 vector[N] Male;
 vector[N] CantWork;
 vector[N] Grip;
 vector[N] Sad;
 vector[N] NoFood;
 vector[N] GoodsValues;
 vector[N] Indigenous;
 vector[N] NotThere;
 vector[N] MissingFocal;
}

parameters {
 vector[P] BG; 
 vector[P] BL; 
 vector[P] BP; 
 vector[P] BT; 
 vector[P] BF; 

 array[N, M] vector[2] FA_raw; 
 array[M] vector<lower=0>[2] FA_SD;
 array[M] cholesky_factor_corr[2] FA_L_chol;

 array[M] matrix[N,N] D_raw;
 array[M] real<lower=0> D_SD;
 array[M] cholesky_factor_corr[2] D_L_chol;
}

model{
//######################################################## Local storage
 real FocalFactors;
 vector[N] TargetFactors;
 row_vector[N] DyadFactors;
 vector[N] Theta;

 array[M] vector[N] F;
 array[M] vector[N] A;

 array[N, M] vector[2] FA;  
 array[M] matrix[N,N] D;
 vector[2] scrap;

//######################################################## Priors
 BG ~ normal(0,1.5);
 
 BL ~ normal(0,1.5);

 BP ~ normal(0,1.5);
 
 BT ~ normal(0,1.5);

 BF ~ normal(0,1.5);

 for(i in 1:N){
 for(m in 1:M){
  FA_raw[i,m] ~ normal(0,1);
 }}

 for(m in 1:M){
  to_vector(D_raw[m]) ~ normal(0,1);
 }

for(m in 1:M){
 FA_SD[m] ~ exponential(1.5);
 FA_L_chol[m] ~ lkj_corr_cholesky(2);
}

 for(m in 1:M){
 D_SD[m] ~ exponential(1.5);
 D_L_chol[m] ~ lkj_corr_cholesky(2);
}

//######################################################## Transformed Priors


 for(i in 1:N){
  for(m in 1:M){
   FA[i,m] = FA_SD[m] .* (FA_L_chol[m]*FA_raw[i, m]);
  }}

 for(m in 1:M){
 for(i in 1:N){
  F[m,i] = FA[i,m,1];
  A[m,i] = FA[i,m,2];
  }}

for(m in 1:M){
   for(i in 1:(N-1)){
   for(j in (i+1):N){  
      scrap[1] = D_raw[m,i,j];
      scrap[2] = D_raw[m,j,i];
      scrap = rep_vector(D_SD[m],2) .* (D_L_chol[m]*scrap);
    D[m,i,j] = scrap[1];           
    D[m,j,i] = scrap[2];                       
    }}
    
   for(i in 1:N){
    D[m,i,i] = -99;                                
    }
  }


//######################################################## Model Allocation Data
 for(i in 1:N){
  if(MissingFocal[i]==0){
  FocalFactors  = Q[1]*(BG[2]*Age[i] + BG[3]*Male[i] + BG[5]*CantWork[i] + BG[6]*Grip[i] + BG[7]*Sad[i] + BG[8]*NoFood[i] + BG[9]*GoodsValues[i]) + 
                  Q[2]*(BG[4]*Indigenous[i]) + 
                  BG[1] + F[1,i];  
  
  TargetFactors = Q[1]*(BG[10]*Age + BG[11]*Male + BG[12]*CantWork + BG[13]*Grip + BG[14]*Sad + BG[15]*NoFood + BG[16]*GoodsValues + BG[17]*NotThere) +
                  A[1];  
    
  DyadFactors   = Q[1]*(BG[18]*Relatedness[i] + BG[19]*Marriage[i] + BG[20]*SameSex[i]) +
                  Q[2]*(BG[21]*SameEthnicity[i]*(1-Indigenous[i]) + BG[22]*SameEthnicity[i]*Indigenous[i]) + 
                  D[1][i];

  Giving[i] ~ multinomial(softmax((FocalFactors + TargetFactors + to_vector(DyadFactors)) .* to_vector(Other[i])));
  }
 }  
 
//######################################################## Model Taking Data
 for(i in 1:N){
     if(MissingFocal[i]==0){
  FocalFactors  = Q[1]*(BL[2]*Age[i] + BL[3]*Male[i] + BL[5]*CantWork[i] + BL[6]*Grip[i] + BL[7]*Sad[i] + BL[8]*NoFood[i] + BL[9]*GoodsValues[i]) + 
                  Q[2]*(BL[4]*Indigenous[i]) + 
                  BL[1] + F[2,i];  
  
  TargetFactors = Q[1]*(BL[10]*Age + BL[11]*Male + BL[12]*CantWork + BL[13]*Grip + BL[14]*Sad + BL[15]*NoFood + BL[16]*GoodsValues + BL[17]*NotThere) +
                  A[2];  
    
  DyadFactors   = Q[1]*(BL[18]*Relatedness[i] + BL[19]*Marriage[i] + BL[20]*SameSex[i]) +
                  Q[2]*(BL[21]*SameEthnicity[i]*(1-Indigenous[i]) + BL[22]*SameEthnicity[i]*Indigenous[i]) + 
                  D[2][i];

  Theta = softmax((FocalFactors + TargetFactors + to_vector(DyadFactors)) .* to_vector(Other[i]));
  Taking[i] ~ multinomial(Theta);  
  }
 }   
 
//######################################################## Model Punishment Data
 for(i in 1:N){
     if(MissingFocal[i]==0){
  FocalFactors  = Q[1]*(BP[2]*Age[i] + BP[3]*Male[i] + BP[5]*CantWork[i] + BP[6]*Grip[i] + BP[7]*Sad[i] + BP[8]*NoFood[i] + BP[9]*GoodsValues[i]) + 
                  Q[2]*(BP[4]*Indigenous[i]) + 
                  BP[1] + F[3,i];  
  
  TargetFactors = Q[1]*(BP[10]*Age + BP[11]*Male + BP[12]*CantWork + BP[13]*Grip + BP[14]*Sad + BP[15]*NoFood + BP[16]*GoodsValues + BP[17]*NotThere) +
                  A[3];  
    
  DyadFactors   = Q[1]*(BP[18]*Relatedness[i] + BP[19]*Marriage[i] + BP[20]*SameSex[i]) +
                  Q[2]*(BP[21]*SameEthnicity[i]*(1-Indigenous[i]) + BP[22]*SameEthnicity[i]*Indigenous[i]) + 
                  D[3][i];

  Reducing[i] ~ multinomial(softmax((FocalFactors + TargetFactors + to_vector(DyadFactors)) .* to_vector(Other[i])));
 }     
 }
 
//######################################################## Model Transfer Data
 for(i in 1:N){
      if(MissingFocal[i]==0){
  FocalFactors  = Q[1]*(BT[2]*Age[i] + BT[3]*Male[i] + BT[5]*CantWork[i] + BT[6]*Grip[i] + BT[7]*Sad[i] + BT[8]*NoFood[i] + BT[9]*GoodsValues[i]) + 
                  Q[2]*(BT[4]*Indigenous[i]) + 
                  BT[1] + F[4,i];  
  
  TargetFactors = Q[1]*(BT[10]*Age + BT[11]*Male + BT[12]*CantWork + BT[13]*Grip + BT[14]*Sad + BT[15]*NoFood + BT[16]*GoodsValues + BT[17]*NotThere) + 
                  A[4];  
    
  DyadFactors   = Q[1]*(BT[18]*Relatedness[i] + BT[19]*Marriage[i] + BT[20]*SameSex[i]) +
                  Q[2]*(BT[21]*SameEthnicity[i]*(1-Indigenous[i]) + BT[22]*SameEthnicity[i]*Indigenous[i]) + 
                  D[4][i];
  
  Theta = softmax((FocalFactors + TargetFactors + to_vector(DyadFactors)) .* to_vector(Other[i]));
  Transfer[i] ~ multinomial(Theta);
   }
 }   

//######################################################## Model Friendship Data
 for(i in 1:N){
      if(MissingFocal[i]==0){
  FocalFactors  = Q[1]*(BF[2]*Age[i] + BF[3]*Male[i] + BF[5]*CantWork[i] + BF[6]*Grip[i] + BF[7]*Sad[i] + BF[8]*NoFood[i] + BF[9]*GoodsValues[i]) + 
                  Q[2]*(BF[4]*Indigenous[i]) + 
                  BF[1] + F[5,i];  
  
  TargetFactors = Q[1]*(BF[10]*Age + BF[11]*Male + BF[12]*CantWork + BF[13]*Grip + BF[14]*Sad + BF[15]*NoFood + BF[16]*GoodsValues + BF[17]*NotThere) +
                  A[5];  
    
  DyadFactors   = Q[1]*(BF[18]*Relatedness[i] + BF[19]*Marriage[i] + BF[20]*SameSex[i]) +
                  Q[2]*(BF[21]*SameEthnicity[i]*(1-Indigenous[i]) + BF[22]*SameEthnicity[i]*Indigenous[i]) + 
                  D[5][i];
  
  Theta = softmax((FocalFactors + TargetFactors + to_vector(DyadFactors)) .* to_vector(Other[i]));
  Friends[i] ~ multinomial(Theta);
   }
 } 

}
                                                     
generated quantities{
 array[M] matrix[2,2] FA_corr;  
 array[M] matrix[2,2] D_corr;
 array[M] real FA_Rho;
 array[M] real D_Rho;

 for(m in 1:M){
  FA_corr[m] = tcrossprod(FA_L_chol[m]); 
  D_corr[m] = tcrossprod(D_L_chol[m]); 
  FA_Rho[m] = FA_corr[m,1,2]; 
  D_Rho[m] = D_corr[m,1,2];
 }
}   
                 
               
 