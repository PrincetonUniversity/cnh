% Toy space model 

clear all

% House keeping
N = 50;    % number agents
worldsize = 100;    
maxtime = 500;
prob_move = 0.5;    % probability of moving next time step
move_coordinates = [1 -1];
F = 20;     % food items
world = zeros(worldsize);

L = zeros(N,2);        % location for each agent, x-y coordinates 
L_f = zeros(F,2);       % generate random locations for food
%%
% Initalize 
% randomly populate location (L)
L(:,1) = randi(100,N,1);    % filling x coordinates
L(:,2) = randi(100,N,1);    % filling y coordinates

L_f(:,1) = randi(100,F,1);
L_f(:,2) = randi(100,F,1);

%%
% putting food into world (matrix of 0,1s)
for x = 1:length(world)
    for y = 1:length(world)
        if isempty(find(L_f(:,1)==x)) == 0  % if there is food at x in L_f
            location = find(L_f(:,1) == x); % then locations are the linear index
            if isempty(find(L_f(location,2) ==y)) == 0  % if there is food at y's where the x's have food
                world(x,y) = 1; % then block world to have food at that set of x and ys. This means that there's no overlap of food. Just a 1 or 0 if food is there/isn't
            end
        end
    end
end

%%

for t = 1:maxtime
    
    for i = 1:N
        % Am I on top of food?
        [state] = checklocation([L(i,1) L(i,2)],world);
        if state == 1;
            world(L(i,2),L(i,1))=0; % if agent on food, food disappears
        end
      
        % Moving agents
        dice1 = rand;   
        dice2 = rand;
        
        % moving x coordinate
        if dice1 < prob_move
            ind_x = randi(2,1,1);  % create random index, can move +/- 1
            L(i,1) = L(i,1) + move_coordinates(ind_x);
        end
        
        % periodic boundaries for x coordinate
        if L(i,1) == 101
            L(i,1) = 1;
        elseif L(i,1) == 0
                L(i,1) = 100;
        end
        
        % moving y coordinate
        if dice2 < prob_move
           ind_y = randi(2,1,1);
           L(i,2) = L(i,2) + move_coordinates(ind_y);
        end
        
        % periodic boundary for y coordinate
        if L(i,2) == 101
            L(i,2) = 1;
        elseif L(i,2) == 0
                L(i,2) = 100;
        end
        
        % setting state back to 0 before next agent
        state = 0;

        end
      
    pcolor(world)
    hold on
    scatter(L(:,1),L(:,2),'.w')
    xlim([1,100]),ylim([1,100])
    hold off
    pause(0.1)
    disp('Time='),disp(t)
    end

          