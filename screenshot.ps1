# name of the image has been passed as $Args[0]
$imageName = $Args[0]
Add-Type -AssemblyName System.Windows.Forms;
$wait = 100
$bak = [System.Windows.Forms.Clipboard]::GetDataObject();
[System.Windows.Forms.Clipboard]::Clear();
Start-Process explorer -ArgumentList 'ms-screenclip:' -Wait;
# Make sure image makes it onto a clipboard
while (-not ([System.Windows.Forms.Clipboard]::ContainsImage())) {
    Start-Sleep -Milliseconds $wait;
}
if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {
    $image = [System.Windows.Forms.Clipboard]::GetImage();
    [System.Drawing.Bitmap]$image.Save($imageName,[System.Drawing.Imaging.ImageFormat]::Png);
    Write-Output 'clipboard content saved as file';
} else {
    Write-Output 'clipboard does not contain image data'
}
[System.Windows.Forms.Clipboard]::SetDataObject($bak);
