% Toy space model

% STATUS: should figure out what type of statistics are useful from an ABM.
% How many fish each agent gets? How fast they deplete a resource? This has
% know to do with specific questions I want to answer. 
clear all
 
% House keeping
N = 20;    % number agents
worldsize = 50;    
maxtime = 500; 
F = 100;     % number food items
prob_move = 0.8; % probability of moving per time step in one dimension (note then that the *actual* p(stay still) is 0.2)
fish_radius = 2; % radius of perception for fish
boundary = 'periodic';

world = zeros(worldsize); 
L = zeros(N,2);        % location for each agent, x-y coordinates 
L_f = zeros(F,2);       % generate random locations for food
 
% Initalize 
% randomly populate location (L)
L(:,1) = randi(worldsize,N,1);    % filling x coordinates
L(:,2) = randi(worldsize,N,1);    % filling y coordinates
L_f(:,1) = randi(worldsize,F,1); 
L_f(:,2) = randi(worldsize,F,1);
 
% Drop food on the world
for x = 1:length(world)
    for y = 1:length(world)
        if isempty(find(L_f(:,1)==x)) == 0
            location = find(L_f(:,1) == x);
            if isempty(find(L_f(location,2) ==y)) == 0
                world(y,x) = 1;
            end
        end
    end
end 


for t = 1:maxtime
    
    for i = 1:N
        % Am I on top of food?
        [state] = checklocation([L(i,1) L(i,2)],world);
        % What fish can I see? (defined relative to me)
        fish_view = fish_check(fish_radius, world, [L(i,1), L(i,2)], worldsize);
        
        if state == 1; % if I am on top of food
            world(L(i,2),L(i,1))=0; % food disappears, you stay put. Yum yum yum.
            state = 0; % reset
        elseif fish_view(1,1) == 0 && fish_view(1,2) == 0 % this is an indicator that I can't see anything (see function)
            [L(i,1) L(i,2)] = move_agents(L(i,1), L(i,2), prob_move); % move randomly

        else % I can see fish! I can see fish!
             [L(i,1) L(i,2)] = chase_fish(L(i,1), L(i,2), fish_view); % move towards fish
        end
       
        clear fish_view
        
        [L(i,1) L(i,2)] = bound(boundary, L(i,1), L(i,2), worldsize);
       
    end
    
    % visualize
    new_world = zeros(worldsize+1, worldsize+1); % just deals with pcolor
    for a=1:worldsize
        new_world(a,1:worldsize) = world(a,:);
    end
    plot_world(new_world, L, t);
    disp(['Time = ' num2str(t)])
end