function P = sub_hunt(P,R,F)

% Go for a walk and avoid the sleeping predator
t = 1;
END = 750;
 while t <= END;
   figure(1);
   p = plot(R.x,R.y,'bo',F.x,F.y,'ro');
   set(p(1),'markerfacecolor','b','markersize',8);
   set(p(2),'markerfacecolor','r','markersize',8);
   axis([0 P.Dx 0 P.Dy]);

   % PERIODIC boundary
   R.x = mod(R.x,P.Dx);   
   R.y = mod(R.y,P.Dy);   
   F.x = mod(F.x,P.Dx);   
   F.y = mod(F.y,P.Dy);   

   % Load/save every step, dont let vectors get big
   eval(['save ./Data/Data_' num2str(t) ' R F P']);
   t = t + 1;

   % DISTANCES between preds and prey (HIGHLY OPTIMIZED AND VERY CONFUSING)
   [T D] = sub_distance(P,R,F);

   % PERCEPTION  
   [R F] = sub_perception(P,R,F,T,D);

   % MOVEMENT
   [R F] = sub_movement(P,R,F);

   % UPDATE
   R.x = R.x + (P.dt.*(R.U+R.u));
   R.y = R.y + (P.dt.*(R.V+R.v));

   F.x = F.x + (P.dt.*(F.U+F.u));
   F.y = F.y + (P.dt.*(F.V+F.v));

   % SPIN UP DEMOGRAPHICS
   [R.Fd Ri] = min(D,[],2);   
   i = Ri(R.Fd < F.G);
   i = unique(i);
   R.x(i) = []; %<- Pred kills prey
   R.y(i) = [];
   R.u(i) = [];
   R.v(i) = [];

   P.attack(t-1) = length(i);%<-RECORD NUMBER OF PREY EATEN

   k   = length(i); % Birth
   %R.x = [R.x; rand(k,1).*P.Dx]; % at randomly placed location
   %R.y = [R.y; rand(k,1).*P.Dy]; 
   R.x = [R.x; R.x(randsample(1:length(R.x),k))]; % at location of a mother
   R.y = [R.y; R.y(randsample(1:length(R.y),k))];
   R.u = [R.u; zeros(k,1)];
   R.v = [R.v; zeros(k,1)];

   disp([num2str(t) ' , ' num2str(END)]);
end 

