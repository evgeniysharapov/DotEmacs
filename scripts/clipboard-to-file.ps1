# 
# If clipboard contains an image then writes clipboard content into the filee 
#
#    name of the file is passed as $Args[0]
#
$imageName = $Args[0]
Add-Type -AssemblyName System.Windows.Forms;
if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {
    $image = [System.Windows.Forms.Clipboard]::GetImage();
    [System.Drawing.Bitmap]$image.Save($imageName,[System.Drawing.Imaging.ImageFormat]::Png);
    Write-Output 'clipboard content saved as file';
} else {
    Write-Output 'clipboard does not contain image data'
}
