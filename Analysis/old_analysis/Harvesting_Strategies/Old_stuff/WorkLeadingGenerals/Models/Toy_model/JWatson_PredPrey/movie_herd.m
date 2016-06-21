


for i = 1:750;
  eval(['load Data/Data_' num2str(i)]);
  figure('visible','off','units','inches','position',[5 8 7 6]), hold on;
  plot(R.x,R.y,'o','markerfacecolor','b','markeredgecolor','b','markersize',8);
  plot(F.x,F.y,'o','markerfacecolor','r','markeredgecolor','r','markersize',9);
  legend('Rabbit','Fox','fontsize',18);
  xlabel('Farm x-axis (ft)','fontsize',18);
  ylabel('Farm y-axis (ft)','fontsize',18);
  title(['t = ' num2str(i) ', R = ' num2str(size(R.x,1)) ', F = ' num2str(size(F.x,1))],'fontsize',18);
  set(gca,'fontsize',15);
  t = num2str(i+1000);
  disp([num2str(i)]);
  axis([0 P.Dx 0 P.Dy]);
  box on;
  set(gcf, 'PaperPositionMode','auto');
  eval(['print -dpdf Figs/Fig_' t(2:end)]);
  close all;
end






