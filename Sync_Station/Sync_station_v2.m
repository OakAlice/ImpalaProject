function varargout = Sync_station_v2(varargin)
    % SYNC_STATION_V2 MATLAB code for Sync_station_v2.fig
    %      This code is an updated version of the accelerometer annotation
    %      software designed for Galea et al., 2021 by Chris Clemente. The
    %      function of this program is to annotate a raw accelerometer trace
    %      with the behaviours it contains based on concurrently filmed video.
    %      This version of the code does not syncronise the content and assumes
    %      this has been synchronised in R (delay setting for minor tweaks
    %      only) as well as allowing for multiple behavioural types to be
    %      added simultaneously. Updates by Oakleigh Wilson, May 2025.
    
    
    % --- Basic set up (not to be changed) ------------------------------------
    % Begin initialization code - DO NOT EDIT
    gui_Singleton = 1;
    gui_State = struct('gui_Name',       mfilename, ...
                       'gui_Singleton',  gui_Singleton, ...
                       'gui_OpeningFcn', @Sync_station_v2_OpeningFcn, ...
                       'gui_OutputFcn',  @Sync_station_v2_OutputFcn, ...
                       'gui_LayoutFcn',  [] , ...
                       'gui_Callback',   []);
    if nargin && ischar(varargin{1})
        gui_State.gui_Callback = str2func(varargin{1});
    end
    
    if nargout
        [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
    else
        gui_mainfcn(gui_State, varargin{:});
    end
    % End initialization code - DO NOT EDIT

function Sync_station_v2_OpeningFcn(hObject, eventdata, handles, varargin)
    % Choose default command line output for Sync_station_v2
    handles.output = hObject;
    
    % Update handles structure
    guidata(hObject, handles);

% --- Outputs from this function are returned to the command line.
function varargout = Sync_station_v2_OutputFcn(hObject, eventdata, handles) 
    % Get default command line output from handles structure
    varargout{1} = handles.output; 

% --- Setting up displays -------------------------------------------------

function mydisplay(hObject, eventdata, handles)
    % main update screen function
    
    axes(handles.axes1);
    
    %% open and show video frame
    if ~isempty(handles.videofile) 
        if strcmp(handles.ext,'.avi') || strcmp(handles.ext,'.AVI')|| strcmp(handles.ext,'.MP4') || strcmp(handles.ext,'.MPG') || strcmp(handles.ext,'.MOV')
                mov = read(handles.video, handles.frame);
        end
        imshow(mov);
        pause(1/handles.framerate);
    end

    guidata(hObject,handles)

% function for displaying the acclerometer
function mydisplay2(hObject, eventdata, handles)

    axes(handles.axes2)
    
    framerate=str2double(get(handles.edit4_getframe, 'String'));
    samplingF=str2double(get(handles.edit_accelrate, 'String'));
    handles.Cfact=samplingF/framerate;
    
        delay=str2double(get(handles.edit_delay,'String'));
        time_sec=round(handles.frame*handles.Cfact-delay+handles.start);
    
    
    if get(handles.radiobutton1_zoom,'Value')==0
        % displaying and colouring each of the axes
        plot(handles.accel_chunk(:,2),'b')
        hold on
        plot(handles.accel_chunk(:,3),'r')
        plot(handles.accel_chunk(:,4),'g')
        hold off
        
         vline(time_sec)
    
    else
        plot(handles.accel_chunk(handles.start_zoom:handles.end_zoom,2),'b')
        hold on
        plot(handles.accel_chunk(handles.start_zoom:handles.end_zoom,3),'r')
        plot(handles.accel_chunk(handles.start_zoom:handles.end_zoom,4),'g')
        hold off  
        
        vline(time_sec-handles.start_zoom)
    end
    guidata(hObject,handles)

% function for displaying the acclerometer
    function mydisplay3(hObject, eventdata, handles)
    axes(handles.axes3)
    
    framerate=str2double(get(handles.edit4_getframe, 'String'));
    samplingF=str2double(get(handles.edit_accelrate, 'String'));
    handles.Cfact=samplingF/framerate;
    
    delay=str2double(get(handles.edit_delay,'String'));
    time_sec=round(handles.frame*handles.Cfact-delay+handles.start);
    
    %%% Add in radiobutton to change which of the behaviour types are
    %%% displayed!!! 
    %%% Or display them all simultaneously???
    mech_behaviour = handles.mech_behaviours;
    eco_behaviour = handles.eco_behaviours;
    
    % displaying and colouring each of the axes
    plot(handles.accel_chunk(:,2),'k')
    hold on
    plot(mech_behaviour,'r')  % Changed from handles.behaviours to behaviorTrace
    plot(eco_behaviour, 'g')
    hold off
    vline(time_sec)

    guidata(hObject,handles)


% --- playing the video --------------------------------------------------- 

% --- Executes on button press in pushbutton1_video.

function pushbutton2_accel_Callback(hObject, eventdata, handles)
    set(handles.edit_delay,'String','0')
    set(handles.radiobutton1_zoom,'Value',0)
    handles.start = 1;
    
    [handles.accelfilename, handles.pathname]=uigetfile('*.csv','pick file');
    if isequal(handles.accelfilename, 0)
        % User canceled
        return;
    end
    
    handles.accelfile = fullfile(handles.pathname,handles.accelfilename);
    
    % More robust file loading
    try
        T = readtable(handles.accelfile);
        accel_chunk = T{:, 1:4};
    catch
        errordlg('Could not read the accelerometer file or required columns are missing.', 'File Error');
        return;
    end
    
    % Make sure we have at least 4 columns
    if size(accel_chunk, 2) < 4
        fprintf('Number of columns in accel_chunk: %d\n', size(accel_chunk, 2));
        errordlg('Accelerometer data must have at least 4 columns (time, x, y, z)', 'Invalid Data');
        return;
    end
    
    handles.accel_chunk = accel_chunk;
    
    try
        set(handles.edit2,'String', datestr(accel_chunk(1,1)));
    catch
        set(handles.edit2,'String', 'Unknown Date');
    end
    
    % Create behavior vectors
    n = size(accel_chunk, 1);
    handles.eco_behaviours = zeros(n, 1);
    handles.func_behaviours = zeros(n, 1);
    handles.mech_behaviours = zeros(n, 1);
    
    guidata(hObject, handles);
    mydisplay2(hObject, eventdata, handles);
    mydisplay3(hObject, eventdata, handles);

% --- Executes on button press in pushbutton3_forward.
function pushbutton3_forward_Callback(hObject, eventdata, handles)
    global stop
    stop = true;
    
    %axes(handles.axes1)
    handles.stop = 0;
    
    step=str2double(get(handles.edit5_frame_step, 'String')); 
    handles.frame=str2double(get(handles.edit_framenum, 'String')); 
    
    while(1)
        if stop == false
           break;
        end
    
        handles.frame = handles.frame+step;
        set(handles.slider1,'Value',handles.frame);
        set(handles.edit_framenum,'String',num2str(handles.frame));
        
        if handles.frame>handles.totalframes
             break;
        end
        
        mydisplay(hObject, eventdata, handles)
        mydisplay2(hObject, eventdata, handles)
        mydisplay3(hObject, eventdata, handles)
    end
    
% --- Executes on button press in pushbutton4_stop.
function pushbutton4_stop_Callback(hObject, eventdata, handles)
 global stop
     stop=false;

% --- setting delay and aligning sources ----------------------------------
function pushbutton_setdelay_Callback(hObject, eventdata, handles)

    [x, ~] = ginput(1);
    xmin1 = max(round(x(1)), 1);

    framerate = str2double(get(handles.edit4_getframe, 'String'));
    samplingF = str2double(get(handles.edit_accelrate, 'String'));
    handles.Cfact = samplingF / framerate;

    % Calculate delay depending on zoom state
    current_time = round(handles.frame * handles.Cfact + handles.start);

    if get(handles.radiobutton1_zoom, 'Value') == 0
        delay = current_time - xmin1;
    else
        delay = current_time - (handles.start_zoom + xmin1);
    end

    set(handles.edit_delay, 'String', num2str(delay));
    guidata(hObject, handles);
    mydisplay2(hObject, eventdata, handles);

% --- Zooming in on specific sections -------------------------------------
function pushbutton_zoom_Callback(hObject, eventdata, handles)

    [x, ~] = ginput(2);
    bounds = round(sort(x));
    xmin1 = max(bounds(1), 1);
    xmax1 = bounds(2);

    % Activate zoom and save bounds
    set(handles.radiobutton1_zoom, 'Value', 1);
    handles.start_zoom = xmin1;
    handles.end_zoom = xmax1;

    guidata(hObject, handles);
    mydisplay2(hObject, eventdata, handles);

% --- General load and tag functions for all the behaviour types ----------
    function load_behaviour_file(hObject, handles, behType)
    [file, path] = uigetfile('*.csv', 'Pick file');
    if isequal(file, 0)
        return;
    end
    behfile = fullfile(path, file);
    beh = readtable(behfile);
    
    % Update handles and correct table
    switch behType
        case 'eco'
            handles.eco_behfile = behfile;
            handles.eco_beh = beh;
            set(handles.uitable1, 'Data', table2cell(beh));
            set(handles.uitable1, 'ColumnName', {'#','Beh'});
        case 'func'
            handles.func_behfile = behfile;
            handles.func_beh = beh;
            set(handles.uitable2, 'Data', table2cell(beh));
            set(handles.uitable2, 'ColumnName', {'#','Beh'});
        case 'mech'
            handles.mech_behfile = behfile;
            handles.mech_beh = beh;
            set(handles.uitable3, 'Data', table2cell(beh));
            set(handles.uitable3, 'ColumnName', {'#','Beh'});
    end

    guidata(hObject, handles);

% behaviours
function tag_behaviour(hObject, eventdata, handles, behnum, behType)
    [x, ~] = ginput(2);
    bounds = round(sort(x));
    xmin1 = max(bounds(1), 1);
    xmax1 = bounds(2);

    if get(handles.radiobutton1_zoom, 'Value') == 0
        idx = xmin1:xmax1;
    else
        idx = handles.start_zoom + xmin1 : handles.start_zoom + xmax1;
    end

    % Annotate the appropriate behaviour track
    switch behType
        case 'eco'
            handles.eco_behaviours(idx) = behnum;
        case 'func'
            handles.func_behaviours(idx) = behnum;
        case 'mech'
            handles.mech_behaviours(idx) = behnum;
    end

    guidata(hObject, handles);
    mydisplay3(hObject, eventdata, handles);


% --- Execute the loading and tagging for 3 types -------------------------
% --- Load behaviours
function pushbutton7_Callback(hObject, eventdata, handles)
    load_behaviour_file(hObject, handles, 'eco');

function pushbutton10_Callback(hObject, eventdata, handles)
    load_behaviour_file(hObject, handles, 'func');

function pushbutton12_Callback(hObject, eventdata, handles)
    load_behaviour_file(hObject, handles, 'mech');

% --- Tag the entered behaviours ------------------------------------------
function pushbutton_tagbeh_Callback(hObject, eventdata, handles)
    behnum = str2double(get(handles.edit_behnum, 'String'));
    tag_behaviour(hObject, eventdata, handles, behnum, 'eco');

function pushbutton11_Callback(hObject, eventdata, handles)
    behnum = str2double(get(handles.edit11, 'String'));
    tag_behaviour(hObject, eventdata, handles, behnum, 'func');

function pushbutton13_Callback(hObject, eventdata, handles)
    behnum = str2double(get(handles.edit12, 'String'));
    tag_behaviour(hObject, eventdata, handles, behnum, 'mech');

% --- Saving the files ----------------------------------------------------
function pushbutton_save_Callback(hObject, eventdata, handles)

    % Get the output filename
    filename = get(handles.edit1, 'String');
    newStr = extractBefore(filename, '.');
    
    % Ensure the pathname ends with a file separator
    if ~endsWith(handles.pathname, filesep)
        handles.pathname = [handles.pathname, filesep];
    end
    
    outfile = [handles.pathname, newStr, '_tagged.csv'];

    % Extract relevant data
    time = handles.accel_chunk(:,1);
    x = handles.accel_chunk(:,2);
    y = handles.accel_chunk(:,3); 
    z = handles.accel_chunk(:,4); 
    
    % Ensure the behaviour annotations exist
    if ~isfield(handles, 'eco_behaviours')
        handles.eco_behaviours = zeros(size(time));
    end
    if ~isfield(handles, 'func_behaviours')
        handles.func_behaviours = zeros(size(time));
    end
    if ~isfield(handles, 'mech_behaviours')
        handles.mech_behaviours = zeros(size(time));
    end

    % Create the table
    tableout = table(time, x, y, z, ...
        handles.eco_behaviours(:), ...
        handles.func_behaviours(:), ...
        handles.mech_behaviours(:), ...
        'VariableNames', {'time', 'x', 'y', 'z', 'eco_behaviour', 'func_behaviour', 'mech_behaviour'});

    % Write it
    writetable(tableout, outfile);

    fprintf('Finished writing accel file to:\n%s\n', outfile);

%radio buttons
function radiobutton1_zoom_Callback(hObject, eventdata, handles)
    guidata(hObject,handles)
    mydisplay2(hObject, eventdata, handles)
    mydisplay3(hObject, eventdata, handles)
     
% --- Slider for moving through the videos --------------------------------
function slider1_Callback(hObject, eventdata, handles)
    handles.frame=round(get(handles.slider1,'Value'));
    
    if handles.frame>handles.totalframes
        handles.frame=handles.totalframes;
    elseif handles.frame<1
        handles.frame=1;
    end

    set(handles.slider1,'Value',handles.frame);
    set(handles.edit_framenum,'String',num2str(handles.frame));
    
    guidata(hObject,handles);
    mydisplay(hObject, eventdata, handles);
    mydisplay2(hObject, eventdata, handles);
    mydisplay3(hObject, eventdata, handles)

% --- Executes during object creation, after setting all properties.
function slider1_CreateFcn(hObject, eventdata, handles)
    if isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
        set(hObject,'BackgroundColor',[.9 .9 .9]);
    end

% --- Other buttons -------------------------------------------------------
function edit_Callback(hObject, ~, handles, tag)
    value = str2double(get(hObject, 'String'));
    handles.(tag) = value;
    guidata(hObject, handles);
    if strcmp(tag, 'edit_framenum')
        mydisplay(hObject, [], handles);
    end

function edit_CreateFcn(hObject, ~, ~)
    if ispc && isequal(get(hObject, 'BackgroundColor'), get(0, 'defaultUicontrolBackgroundColor'))
        set(hObject, 'BackgroundColor', 'white');
    end

function hhh = vline(x, in1, in2)
    if numel(x) > 1
        for I = 1:numel(x)
            linetype = getParam(in1, I, 'r:');
            label = getParam(in2, I, '');
            h(I) = vline(x(I), linetype, label);
        end
    else
        if nargin < 2, in1 = 'r:'; end
        if nargin < 3, in2 = ''; end
        linetype = in1;
        label = in2;
        g = ishold(gca);
        hold on;
        y = get(gca, 'ylim');
        h = plot([x x], y, linetype);
        if ~isempty(label)
            xx = get(gca, 'xlim');
            xrange = diff(xx);
            xunit = (x - xx(1)) / xrange;
            offset = 0.01 * xrange * (xunit < 0.8) - 0.05 * xrange * (xunit >= 0.8);
            text(x + offset, y(1) + 0.1 * diff(y), label, 'color', get(h, 'color'));
        end
        if ~g, hold off; end
        set(h, 'tag', 'vline', 'handlevisibility', 'off');
    end
    if nargout, hhh = h; end

function param = getParam(input, idx, default)
    if nargin < 1 || isempty(input)
        param = default;
    elseif iscell(input)
        param = input{min(idx, end)};
    else
        param = input;
    end

% --- create functions for these new buttons ------------------------------
function edit_common_CreateFcn(hObject, ~, ~)
    if ispc && isequal(get(hObject, 'BackgroundColor'), get(0, 'defaultUicontrolBackgroundColor'))
        set(hObject, 'BackgroundColor', 'white');
    end

function edit12_CreateFcn(hObject, eventdata, handles)
    edit_common_CreateFcn(hObject, eventdata, handles);

function edit11_CreateFcn(hObject, eventdata, handles)
    edit_common_CreateFcn(hObject, eventdata, handles);

function edit_behnum_CreateFcn(hObject, eventdata, handles)
    edit_common_CreateFcn(hObject, eventdata, handles);

function edit_delay_CreateFcn(hObject, eventdata, handles)
    edit_common_CreateFcn(hObject, eventdata, handles);

function edit_accelrate_CreateFcn(hObject, eventdata, handles)
    edit_common_CreateFcn(hObject, eventdata, handles);

function edit_framenum_CreateFcn(hObject, eventdata, handles)
    edit_common_CreateFcn(hObject, eventdata, handles);

function edit_frame_step_CreateFcn(hObject, eventdata, handles)
    edit_common_CreateFcn(hObject, eventdata, handles);

function edit4_getframe_CreateFcn(hObject, eventdata, handles)
    edit_common_CreateFcn(hObject, eventdata, handles);

function edit2_CreateFcn(hObject, eventdata, handles)
    edit_common_CreateFcn(hObject, eventdata, handles);

function edit1_CreateFcn(hObject, eventdata, handles)
    edit_common_CreateFcn(hObject, eventdata, handles);

function edit5_frame_step_CreateFcn(hObject, eventdata, handles)
    edit_common_CreateFcn(hObject, eventdata, handles);

function pushbutton1_video_CreateFcn(hObject, eventdata, handles)
    % nothing here

% Frame number edit box
function edit_framenum_Callback(hObject, eventdata, handles)
    frame = str2double(get(hObject, 'String'));
    if isnan(frame)
        set(hObject, 'String', num2str(handles.frame))
        return
    end
    
    % Enforce limits
    if frame > handles.totalframes
        frame = handles.totalframes;
    elseif frame < 1
        frame = 1;
    end
    
    handles.frame = frame;
    set(handles.slider1, 'Value', frame);
    
    guidata(hObject, handles);
    mydisplay(hObject, eventdata, handles);
    mydisplay2(hObject, eventdata, handles);
    mydisplay3(hObject, eventdata, handles);

% Delay edit box
function edit_delay_Callback(hObject, eventdata, handles)
    delay = str2double(get(hObject, 'String'));
    if isnan(delay)
        set(hObject, 'String', '0')
        return
    end
    
    guidata(hObject, handles);
    mydisplay2(hObject, eventdata, handles);
    mydisplay3(hObject, eventdata, handles);

% Accelerometer rate edit box
function edit_accelrate_Callback(hObject, eventdata, handles)
    rate = str2double(get(hObject, 'String'));
    if isnan(rate) || rate <= 0
        set(hObject, 'String', '100') % Default to 100Hz
        return
    end
    
    guidata(hObject, handles);
    mydisplay2(hObject, eventdata, handles);
    mydisplay3(hObject, eventdata, handles);

% Frame step edit box
function edit5_frame_step_Callback(hObject, eventdata, handles)
    step = str2double(get(hObject, 'String'));
    if isnan(step) || step <= 0
        set(hObject, 'String', '1') % Default to step of 1
        return
    end
    
    guidata(hObject, handles);

% Frame rate edit box
function edit4_getframe_Callback(hObject, eventdata, handles)
    framerate = str2double(get(hObject, 'String'));
    if isnan(framerate) || framerate <= 0
        if isfield(handles, 'framerate')
            set(hObject, 'String', num2str(handles.framerate))
        else
            set(hObject, 'String', '30') % Default to 30fps
        end
        return
    end
    
    guidata(hObject, handles);
    mydisplay2(hObject, eventdata, handles);
    mydisplay3(hObject, eventdata, handles);

% Behavior number edit boxes
function edit_behnum_Callback(hObject, eventdata, handles)
    behnum = str2double(get(hObject, 'String'));
    if isnan(behnum) || behnum < 0
        set(hObject, 'String', '1') % Default to behavior 1
    end
    guidata(hObject, handles);

function edit11_Callback(hObject, eventdata, handles)
    behnum = str2double(get(hObject, 'String'));
    if isnan(behnum) || behnum < 0
        set(hObject, 'String', '1') % Default to behavior 1
    end
    guidata(hObject, handles);

function edit12_Callback(hObject, eventdata, handles)
    behnum = str2double(get(hObject, 'String'));
    if isnan(behnum) || behnum < 0
        set(hObject, 'String', '1') % Default to behavior 1
    end
    guidata(hObject, handles);


% --- Executes on button press in pushbutton1_video.
function pushbutton1_video_Callback(hObject, eventdata, handles)
    [handles.videofilename, handles.pathname]=uigetfile({'*.MOV;*.avi;*.MP4;*.seq','Video files';'*.tif;*.jpg;*.bmp', 'Image files'},'pick file');
    handles.videofile = fullfile(handles.pathname,handles.videofilename);
    [~,handles.name,handles.ext] = fileparts(handles.videofile);
    
    %% MOV files, initialise
        video = VideoReader(handles.videofile);
            lastFrame = read(video, inf);
            numFrames = video.NumberOfFrames;
            handles.video=video;
            handles.totalframes = video.NumberOfFrames;
            handles.height = video.Height;
            handles.width = video.Width;
            handles.white= 2^(video.BitsPerPixel/3)-1; 
            handles.framerate = video.FrameRate;
            set(handles.edit4_getframe,'String',handles.framerate);
            set(handles.slider1,'max',handles.totalframes, 'min',1,'Value',1);
            set(handles.slider1, 'SliderStep', [1/handles.totalframes , 10/handles.totalframes ]);
    set(handles.edit_framenum,'String','1');
    %set(handles.edit2_totalframes,'String',num2str(handles.totalframes));
    set(handles.edit1,'String',handles.videofilename);
    handles.frame=1;
    handles.rect=[];
    handles.stop=0;

    guidata(hObject, handles);
    mydisplay(hObject, eventdata, handles)


% --- Executes during object creation, after setting all properties.
function axes1_CreateFcn(hObject, eventdata, handles)
    % hObject    handle to axes1 (see GCBO)
    % eventdata  reserved - to be defined in a future version of MATLAB
    % handles    empty - handles not created until after all CreateFcns called
    
    % Hint: place code in OpeningFcn to populate axes1