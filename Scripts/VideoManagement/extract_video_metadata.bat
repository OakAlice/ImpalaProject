@echo off
setlocal enabledelayedexpansion

REM Set path to ffprobe.exe
set "ffprobe=C:\Users\PC\Downloads\Software\ffmpeg\bin\ffprobe.exe"

REM Set target directory and output CSV file
set "target_dir=C:\Users\PC\Documents\ImpalaProject\RawData\Collar_2\Videos"
set "output_csv=%target_dir%\video_metadata.csv"

REM Write CSV header
echo FileName,FullPath,Duration_sec,Size_Bytes,Codec,Width,Height,FrameRate,CreationTime > "%output_csv%"

REM Set video extensions to search
set "exts=mp4 avi mov mkv wmv mts"

REM Loop through each extension
for %%e in (%exts%) do (
    REM Recursively find matching files
    for /R "%target_dir%" %%f in (*.%%e) do (
        echo Processing: %%~nxf

        REM Initialize variables
        set "duration="
        set "size="
        set "creation="
        set "codec="
        set "width="
        set "height="
        set "fps="

        REM Get format-level metadata
        for /f "delims=" %%a in ('""%ffprobe%" -v quiet -print_format csv -show_entries format=duration,size:format_tags=creation_time "%%~f""') do (
            set "format_line=%%a"
        )

        REM Strip quotes and parse format line
        set "format_line=!format_line:"=!"
        for /f "tokens=1,2,3 delims=," %%x in ("!format_line!") do (
            set "duration=%%x"
            set "size=%%y"
            set "creation=%%z"
        )

        REM Get first video stream metadata
        for /f "delims=" %%a in ('""%ffprobe%" -v quiet -select_streams v:0 -print_format csv -show_entries stream=codec_name,width,height,r_frame_rate "%%~f""') do (
            set "stream_line=%%a"
        )

        REM Strip quotes and parse stream line
        set "stream_line=!stream_line:"=!"
        for /f "tokens=1,2,3,4 delims=," %%x in ("!stream_line!") do (
            set "codec=%%x"
            set "width=%%y"
            set "height=%%z"
            set "fps=%%w"
        )

        REM Write to CSV
        echo %%~nxf,"%%~f",!duration!,!size!,!codec!,!width!,!height!,!fps!,!creation! >> "%output_csv%"
    )
)

echo Done! Metadata saved to: %output_csv%
pause
