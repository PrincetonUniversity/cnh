function [] = plot_world(world,L,t);
    pcolor(world)
    hold on
    scatter(L(:,1)+0.5,L(:,2)+0.5,'.w'); % adjust the plot so individuals sit on the grid nicely (aesthetics only)
    xlim([1,length(world)]),ylim([1,length(world)])
    title(['Time = ' num2str(t)])
    hold off
    pause(0.1)
end