%clear all;
zmin = 1;
step =1.008;
nz = 200;
f=0;
vz = zeros(nz,1);
s = 0.9;
ce=15;%16;
dbar=1;
alpha=0.6;
r=0.12;
gamma=0.25;

for i=1:nz
     vz(i)=zmin*step;
     zmin=vz(i);
end
N=2;
g=zeros(N,nz);
fmass=zeros(N,nz);
cfmass=zeros(N);

median=zeros(N,1);
me=zeros(N,1);
stdd=zeros(N,1);
lmedian=zeros(N,1);
lmean=zeros(N,1);
lstd=zeros(N,1);
vz = linspace(log(0.0001),8,nz)';
step=(vz(2)-vz(1))/2;
sigmaN=0.5;%0.9;
entme=1;
ini = normcdf(vz+step,entme-sigmaN^2/2,sigmaN) - ...
    normcdf(vz-step,entme-sigmaN^2/2,sigmaN);
ini(1) = normcdf(vz(1)+step,entme-sigmaN^2/2,sigmaN)-normcdf(vz(1)-step,entme-sigmaN^2/2,sigmaN);
ini(end) = normcdf(vz(end)+step,entme-sigmaN^2/2,sigmaN)-normcdf(vz(end)-step,entme-sigmaN^2/2,sigmaN);
%k=0.8;
%sigma=0.8;
%theta=-2;
%ini = gpcdf(vz+step,k,sigma,theta) - ...
%    gpcdf(vz-step,k,sigma,theta);
%ini(1) = gpcdf(vz(1)+step,k,sigma,theta)-gpcdf(vz(1)-step,k,sigma,theta);
%ini(end) = gpcdf(vz(end)+step,k,sigma,theta)-gpcdf(vz(end)-step,k,sigma,theta);
ini = (ini/sum(ini))';
%invar =transmat(vz,1-0.5^2/2,0.5,0.9,0)^10000;
%ini= invar(1,:);
sigmaG=0.4;%.32;
sigmaU=0.33;%0.33;
for n=1:N
beta = 0.96;
 %it seems too large kappa will decrease std to the level of the ini dist

%P = ini;

tolv = 1e-5;
tole=1e-5;
breakw=0;
wu=exp(100); %price upper limit
wl=0;  %price lower limit
w=0.5;
etau=1;
etal=0;
eta=0;
breake=0;

%while ~breake
while ~breakw
    W = zeros(nz,1);
    Wnew = zeros(nz,1);
    Vprime = zeros(nz,1);
    zstar = zeros(nz,1);
    ezstar = zeros(nz,1);
    dzindex=zeros(nz,nz);
    V = zeros(nz);
    breakv = 0;
    P = transmat1(vz,-sigmaG^2/2,sigmaG,1,-eta);
    Pu=transmat1(vz,-sigmaU^2/2,sigmaU,1,0);
    Pz=zeros(nz,nz);
    kappa =1*1.2^(n-1); 



while ~breakv
    for i=1:nz
            %Wp=interp1(exp(vz),W,exp(vz-eta),'pchip');
            V(:,i)=max(W-kappa*(exp(vz)./exp(vz(i))),W(i));
            izindex=find(V(:,i)>W(i),1);
            dzindex(i,:)=(V(:,i)>W(i));
            Pz(i,:)=P(i,:).*dzindex(i,:);
            Pz(i,i)=1-sum(Pz(i,:));
            Vprime(i)=Pz(i,:)*max(0,V(:,i));
            
            
     
         %Pp=pchip(vz,P(i,:),vz./eta)';
        
        if isempty(izindex)
            zstar(i)=max(vz)+0.00001;
        else
           zstar(i)=vz(izindex); %find the innovation cutoff epi'
        end
        
       
         ezindex=find(V(:,i)>0,1);
        if isempty(ezindex)
            ezstar(i)=max(vz)+0.01;
        else
            ezstar(i)=vz(ezindex);%find the exit cutoff z
        end
    end
    
     
     profit=(1-alpha-gamma)*(r^(-gamma) * ...
            w^(-alpha)*gamma^gamma*alpha^alpha)^(1/(1-alpha-gamma)) ...
            .* (exp(vz));
         
         Wnew=profit-f+s*beta*Pu*Vprime; 
         %Wnew(i)=p*vz(i)-f+s*beta*P*V(:,i);  
         
         
        
    
     if max(abs(W-Wnew))<tolv
            breakv=1;
    end
        %max(abs(V-vtom))
        W=Wnew;
end
    v_e=ini*W;
    if abs(v_e-ce)<tole
        breakw=1;
        %breake=1;
    elseif (v_e-ce)>tole
        wl=w;
        w=(wu+wl)/2;
        %etal=eta;
        %eta=(etau+etal)/2;
    elseif (ce-v_e)>tole
        wu=w;
        w=(wu+wl)/2;
        %etau=eta;
        %eta=(etau+etal)/2;
    end
    
    
    
end


x_pol=zeros(nz);
i_pol=zeros(nz);
for i=1:nz
    for j=1:nz
    if vz(j)<ezstar(i)
        x_pol(i,j)=1; %exit if 1
    end
    
    if vz(j)<zstar(i)
        i_pol(i,j)=1;   %not innovate if 1
    end
    end
end


trans = Pz;
 
        %trans(i,:) = interp1(vz'-eta,trans(i,:),vz','pchip',0);
        %trans(trans<1e-4)=0;

 
 %[m1,m2]=meshgrid(vz,vz./eta);
 %[m11,m22]=meshgrid(vz,vz);
 %trans=interp2(m1,m2,trans,m11,m22,'linear',0);
 
 for i = 1:nz
    
                      
        trans(i,:) = s.*(trans(i,:)).*(1-x_pol(i,:)); % excluding exit

 end
 trans=(Pu*trans)'; % Must be transposed
 breakm=0;
 tolm=1e-6;
 mu=1000000; %m upper limit
 ml=0; %m lower limit
 m=10;
 
 %ini=normpdf(vz,30,1)';
while ~breakm
   

    % Initial guess for stationary distribution of incumbent firms
     %probst = (1/(leneps))*ones(leneps,1);

    %test=1;
    %while test > 10^(-4)
    % Apply transition matrix to incumbents + entrants
    %probst1 = trans*probst + entrants*m;
    %test = max(abs(probst1-probst));
    %probst = probst1;
    %test
    %end
    probst=m.*inv(eye(nz)-trans)*ini';
    y=(exp(vz)).* ...
    ((r^(-gamma)*w^(-alpha)*gamma^gamma*alpha^alpha) ...
    * (1).^(alpha+gamma)).^(1/(1-alpha-gamma));
    Y=sum(probst.*y);
    %find the m
    if abs(Y-dbar)<tolm
        breakm=1;
    elseif (Y-dbar)>tolm
        mu=m;
        m=(mu+ml)/2;
    elseif (dbar-Y)>tolm
        ml=m;
        m=(mu+ml)/2;
    end
    
    
end
 
 
 

fmass(n,:)=probst/sum(probst);
me(n)=fmass(n,:)*vz;

%k=find(vz>=vz(200)-eta,1);
        %if abs(mean(vz)-me(n))<tole
          %  breake=1;
        %elseif mean(vz)-me(n)>0
         %   etau=eta;
          %  eta=(etau+etal)/2;
        %elseif mean(vz)-me(n)<0
         %   etal=eta;
          %  eta=(etau+etal)/2;
        %end
%end
 eta
 w

W1=W;
g(n,:)=exp(zstar)./exp(vz);
emp = (exp(vz)).* ...
    (r^(-gamma)*w^(gamma-1)*gamma^gamma*alpha^(1-gamma) ...
    )^(1/(1-alpha-gamma));
%emp=emp./min(emp);
cfmass(n,1)=fmass(n,1);
for i=2:nz
    cfmass(n,i)=cfmass(n,i-1)+fmass(n,i);
end
m=find(cfmass(n,:)>0.5,1);
median(n)=vz(m-1)+(0.5-cfmass(n,m-1))/fmass(n,m)*(vz(m)-vz(m-1));
lmedian(n)=log(vz(m-1))+(0.5-cfmass(n,m-1))/fmass(n,m)*(log(vz(m))-log(vz(m-1)));

me(n)=fmass(n,:)*vz;
lmean(n)=fmass(n,:)*log(vz);

stdd(n)=sqrt(sum(fmass(n,:)*(vz-me(n)).^2));
lstd(n)=sqrt(sum(fmass(n,:)*(log(vz)-lmean(n)).^2));

[ans,PercInd95]=min(abs(cfmass(n,:)-0.95));
nmass95=(fmass(n,PercInd95:end)*emp(PercInd95:end))/(fmass(n,:)*emp);

nmassme=fmass(n,:)*emp;
meanbin=sum(emp<nmassme);
%out.smallshare=sum(fmass((1:meanbin)));
smallshareemp=fmass(n,1:meanbin)*emp(1:meanbin)/nmassme;

targetvec=[0.737 20 0.16];
resvec=[nmass95 (nmassme) smallshareemp]
out=sum(((resvec-targetvec)./targetvec).^2);
jobto=fmass* sum(trans'.*abs(repmat(emp',[nz,1])-repmat(emp,[1,nz])),2)./(fmass*emp);

if n==1
    trans1=trans;
    V1=V;
end
end



