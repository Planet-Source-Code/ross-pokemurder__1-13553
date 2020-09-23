Attribute VB_Name = "Module1"
Declare Function sndPlaySound Lib "winmm.dll" Alias "sndPlaySoundA" (ByVal lpszSoundName As String, ByVal uFlags As Long) As Long
Global Const SND_ASYNC = &H1
Global topscore As Integer
Global out As String
Global myscore As String

Global left1(151), top1(151) As Integer


Function encrypt(data As String) As String
On Error Resume Next
Dim counter, lk, fin As String
For counter = 1 To Len(data)
lk = Mid(data, counter, 1)
lk = Asc(lk)
lk = 473 - lk
If Len(lk) = 1 Then lk = "00" & lk
If Len(lk) = 2 Then lk = "0" & lk
fin = fin & lk
Next counter
out = fin
End Function

Function decrypt(data As String) As String
On Error Resume Next
Dim counter, count2, fin, da As String, lk As Integer
count2 = 1
For counter = 1 To (Len(data) / 3)
lk = Mid(data, count2, 3)
lk = 473 - lk
da = Chr(lk)
fin = fin & da
count2 = count2 + 3
Next counter
out = fin
End Function

Function topscores(named As String)
On Error Resume Next
Dim b, filename, l
filename = App.Path & "\score.dat"
Module1.encrypt named
l = out
Module1.encrypt myscore
b = out
Open filename For Output As #1
Print #1, l
Print #1, b
Close #1
End Function
