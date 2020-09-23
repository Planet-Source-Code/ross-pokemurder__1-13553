VERSION 5.00
Begin VB.Form Form1 
   BackColor       =   &H80000000&
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   8970
   ClientLeft      =   15
   ClientTop       =   15
   ClientWidth     =   11970
   ControlBox      =   0   'False
   Icon            =   "pokemurder.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   MousePointer    =   2  'Cross
   Picture         =   "pokemurder.frx":08CA
   ScaleHeight     =   598
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   798
   StartUpPosition =   3  'Windows Default
   Begin VB.Timer Timer3 
      Interval        =   1000
      Left            =   2520
      Top             =   3960
   End
   Begin VB.Timer Timer2 
      Interval        =   25
      Left            =   2640
      Top             =   3240
   End
   Begin VB.Timer Timer1 
      Enabled         =   0   'False
      Interval        =   25
      Left            =   2760
      Top             =   3240
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   12
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   735
      Left            =   5040
      TabIndex        =   4
      Top             =   4245
      Width           =   2535
   End
   Begin VB.Image Image4 
      Height          =   1065
      Index           =   4
      Left            =   4680
      Picture         =   "pokemurder.frx":23A08
      Stretch         =   -1  'True
      Top             =   4080
      Width           =   3225
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H0000FFFF&
      BorderStyle     =   1  'Fixed Single
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   255
      Index           =   3
      Left            =   7800
      TabIndex        =   3
      Top             =   2400
      Width           =   1215
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H0000FFFF&
      BorderStyle     =   1  'Fixed Single
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   9
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   255
      Index           =   2
      Left            =   8880
      TabIndex        =   2
      Top             =   6480
      Width           =   1215
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H0000FFFF&
      BorderStyle     =   1  'Fixed Single
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   255
      Index           =   1
      Left            =   1320
      TabIndex        =   1
      Top             =   6360
      Width           =   1215
   End
   Begin VB.Image Image4 
      Height          =   1905
      Index           =   3
      Left            =   7440
      Picture         =   "pokemurder.frx":24414
      Top             =   1560
      Width           =   1905
   End
   Begin VB.Image Image4 
      Height          =   1905
      Index           =   2
      Left            =   8520
      Picture         =   "pokemurder.frx":25193
      Top             =   5640
      Width           =   1905
   End
   Begin VB.Image Image4 
      Height          =   1905
      Index           =   1
      Left            =   960
      Picture         =   "pokemurder.frx":25F12
      Top             =   5520
      Width           =   1905
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H0000FFFF&
      BorderStyle     =   1  'Fixed Single
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   255
      Index           =   0
      Left            =   1320
      TabIndex        =   0
      Top             =   1680
      Width           =   1215
   End
   Begin VB.Image Image4 
      Height          =   1905
      Index           =   0
      Left            =   960
      Picture         =   "pokemurder.frx":26C91
      Top             =   840
      Width           =   1905
   End
   Begin VB.Image Image2 
      Height          =   960
      Index           =   4
      Left            =   -720
      Picture         =   "pokemurder.frx":27A10
      Top             =   3480
      Visible         =   0   'False
      Width           =   960
   End
   Begin VB.Image Image2 
      Height          =   960
      Index           =   3
      Left            =   -840
      Picture         =   "pokemurder.frx":28E52
      Top             =   4200
      Visible         =   0   'False
      Width           =   960
   End
   Begin VB.Image Image2 
      Height          =   960
      Index           =   2
      Left            =   -840
      Picture         =   "pokemurder.frx":29544
      Top             =   3600
      Visible         =   0   'False
      Width           =   960
   End
   Begin VB.Image Image2 
      Height          =   960
      Index           =   1
      Left            =   -720
      Picture         =   "pokemurder.frx":29A4C
      Top             =   2400
      Visible         =   0   'False
      Width           =   960
   End
   Begin VB.Image Image2 
      Height          =   960
      Index           =   0
      Left            =   -720
      Picture         =   "pokemurder.frx":29E6C
      Top             =   2520
      Visible         =   0   'False
      Width           =   960
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   151
      Left            =   9240
      Picture         =   "pokemurder.frx":2A222
      Top             =   2520
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   150
      Left            =   8400
      Picture         =   "pokemurder.frx":2A694
      Top             =   240
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   149
      Left            =   5640
      Picture         =   "pokemurder.frx":2AB24
      Top             =   2040
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   148
      Left            =   8280
      Picture         =   "pokemurder.frx":2AFBA
      Top             =   1320
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   147
      Left            =   6000
      Picture         =   "pokemurder.frx":2B42D
      Top             =   240
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   146
      Left            =   8160
      Picture         =   "pokemurder.frx":2B89C
      Top             =   3480
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   145
      Left            =   6840
      Picture         =   "pokemurder.frx":2BDAC
      Top             =   4920
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   144
      Left            =   9840
      Picture         =   "pokemurder.frx":2C277
      Top             =   4200
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   143
      Left            =   8880
      Picture         =   "pokemurder.frx":2C728
      Top             =   4800
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   142
      Left            =   9840
      Picture         =   "pokemurder.frx":2CBF2
      Top             =   1920
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   141
      Left            =   10920
      Picture         =   "pokemurder.frx":2D079
      Top             =   3000
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   140
      Left            =   9360
      Picture         =   "pokemurder.frx":2D57B
      Top             =   3600
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   139
      Left            =   8280
      Picture         =   "pokemurder.frx":2D9D7
      Top             =   2760
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   138
      Left            =   7680
      Picture         =   "pokemurder.frx":2DED0
      Top             =   2040
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   137
      Left            =   7680
      Picture         =   "pokemurder.frx":2E374
      Top             =   3840
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   136
      Left            =   8160
      Picture         =   "pokemurder.frx":2E7C3
      Top             =   5160
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   135
      Left            =   6720
      Picture         =   "pokemurder.frx":2ECA8
      Top             =   2880
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   134
      Left            =   6000
      Picture         =   "pokemurder.frx":2F159
      Top             =   3600
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   133
      Left            =   5160
      Picture         =   "pokemurder.frx":2F61C
      Top             =   3840
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   132
      Left            =   5160
      Picture         =   "pokemurder.frx":2FA91
      Top             =   2880
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   131
      Left            =   6120
      Picture         =   "pokemurder.frx":2FE8F
      Top             =   4920
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   130
      Left            =   6840
      Picture         =   "pokemurder.frx":30339
      Top             =   5880
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   129
      Left            =   5400
      Picture         =   "pokemurder.frx":3082C
      Top             =   5160
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   128
      Left            =   2160
      Picture         =   "pokemurder.frx":30CEF
      Top             =   5040
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   127
      Left            =   5640
      Picture         =   "pokemurder.frx":311BB
      Top             =   4440
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   126
      Left            =   6000
      Picture         =   "pokemurder.frx":31685
      Top             =   5880
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   125
      Left            =   4920
      Picture         =   "pokemurder.frx":31BB4
      Top             =   6000
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   124
      Left            =   8400
      Picture         =   "pokemurder.frx":32092
      Top             =   6120
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   123
      Left            =   9600
      Picture         =   "pokemurder.frx":32594
      Top             =   5880
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   122
      Left            =   10320
      Picture         =   "pokemurder.frx":32A89
      Top             =   5280
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   121
      Left            =   10320
      Picture         =   "pokemurder.frx":32F8B
      Top             =   6480
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   120
      Left            =   10920
      Picture         =   "pokemurder.frx":3341B
      Top             =   4200
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   119
      Left            =   10200
      Picture         =   "pokemurder.frx":33878
      Top             =   3360
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   118
      Left            =   10800
      Picture         =   "pokemurder.frx":33D66
      Top             =   2400
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   117
      Left            =   10800
      Picture         =   "pokemurder.frx":341E5
      Top             =   1320
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   116
      Left            =   8520
      Picture         =   "pokemurder.frx":3465B
      Top             =   1920
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   115
      Left            =   9720
      Picture         =   "pokemurder.frx":34A8C
      Top             =   360
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   114
      Left            =   9840
      Picture         =   "pokemurder.frx":34F97
      Top             =   2760
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   113
      Left            =   8880
      Picture         =   "pokemurder.frx":35440
      Top             =   3360
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   112
      Left            =   10920
      Picture         =   "pokemurder.frx":358A7
      Top             =   4800
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   111
      Left            =   3360
      Picture         =   "pokemurder.frx":35DAB
      Top             =   2640
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   110
      Left            =   6360
      Picture         =   "pokemurder.frx":3625F
      Top             =   1800
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   109
      Left            =   5280
      Picture         =   "pokemurder.frx":3671A
      Top             =   1200
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   108
      Left            =   8520
      Picture         =   "pokemurder.frx":36BD3
      Top             =   4200
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   107
      Left            =   6960
      Picture         =   "pokemurder.frx":37073
      Top             =   6600
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   106
      Left            =   11400
      Picture         =   "pokemurder.frx":37531
      Top             =   1920
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   105
      Left            =   5040
      Picture         =   "pokemurder.frx":379C4
      Top             =   360
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   104
      Left            =   4680
      Picture         =   "pokemurder.frx":37E90
      Top             =   1560
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   103
      Left            =   4320
      Picture         =   "pokemurder.frx":382DE
      Top             =   3120
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   102
      Left            =   4440
      Picture         =   "pokemurder.frx":387BA
      Top             =   3960
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   101
      Left            =   4680
      Picture         =   "pokemurder.frx":38C3B
      Top             =   4680
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   100
      Left            =   3960
      Picture         =   "pokemurder.frx":3906C
      Top             =   1440
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   99
      Left            =   3480
      Picture         =   "pokemurder.frx":39470
      Top             =   960
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   98
      Left            =   3720
      Picture         =   "pokemurder.frx":39958
      Top             =   2160
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   97
      Left            =   3480
      Picture         =   "pokemurder.frx":39DFD
      Top             =   3720
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   96
      Left            =   3720
      Picture         =   "pokemurder.frx":3A2A5
      Top             =   4680
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   95
      Left            =   4320
      Picture         =   "pokemurder.frx":3A730
      Top             =   5400
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   94
      Left            =   3480
      Picture         =   "pokemurder.frx":3AB98
      Top             =   5640
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   93
      Left            =   3000
      Picture         =   "pokemurder.frx":3AFF0
      Top             =   4560
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   92
      Left            =   3000
      Picture         =   "pokemurder.frx":3B45D
      Top             =   1920
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   91
      Left            =   2760
      Picture         =   "pokemurder.frx":3B8A2
      Top             =   3480
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   90
      Left            =   2280
      Picture         =   "pokemurder.frx":3BD8D
      Top             =   2640
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   89
      Left            =   2400
      Picture         =   "pokemurder.frx":3C1F1
      Top             =   1200
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   88
      Left            =   1680
      Picture         =   "pokemurder.frx":3C681
      Top             =   2280
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   87
      Left            =   2040
      Picture         =   "pokemurder.frx":3CB10
      Top             =   3360
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   86
      Left            =   1680
      Picture         =   "pokemurder.frx":3CF68
      Top             =   4080
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   85
      Left            =   1200
      Picture         =   "pokemurder.frx":3D3B7
      Top             =   3000
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   84
      Left            =   2400
      Picture         =   "pokemurder.frx":3D884
      Top             =   4440
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   83
      Left            =   1680
      Picture         =   "pokemurder.frx":3DCE8
      Top             =   1080
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   82
      Left            =   720
      Picture         =   "pokemurder.frx":3E1C6
      Top             =   2280
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   81
      Left            =   600
      Picture         =   "pokemurder.frx":3E684
      Top             =   3480
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   80
      Left            =   1920
      Picture         =   "pokemurder.frx":3EACE
      Top             =   240
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   79
      Left            =   1080
      Picture         =   "pokemurder.frx":3EFF8
      Top             =   480
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   78
      Left            =   360
      Picture         =   "pokemurder.frx":3F495
      Top             =   1560
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   77
      Left            =   840
      Picture         =   "pokemurder.frx":3F947
      Top             =   4320
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   76
      Left            =   1320
      Picture         =   "pokemurder.frx":3FE03
      Top             =   5400
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   75
      Left            =   2760
      Picture         =   "pokemurder.frx":40307
      Top             =   6000
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   74
      Left            =   4200
      Picture         =   "pokemurder.frx":407A7
      Top             =   6480
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   73
      Left            =   480
      Picture         =   "pokemurder.frx":40BC7
      Top             =   5760
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   72
      Left            =   2040
      Picture         =   "pokemurder.frx":41079
      Top             =   6120
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   71
      Left            =   1080
      Picture         =   "pokemurder.frx":41513
      Top             =   6480
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   70
      Left            =   3960
      Picture         =   "pokemurder.frx":419F4
      Top             =   360
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   69
      Left            =   240
      Picture         =   "pokemurder.frx":41EC8
      Top             =   3000
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   68
      Left            =   240
      Picture         =   "pokemurder.frx":4233E
      Top             =   4920
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   67
      Left            =   6480
      Picture         =   "pokemurder.frx":427EF
      Top             =   4200
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   66
      Left            =   3360
      Picture         =   "pokemurder.frx":42C8E
      Top             =   6360
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   65
      Left            =   5520
      Picture         =   "pokemurder.frx":430F4
      Top             =   6360
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   64
      Left            =   6240
      Picture         =   "pokemurder.frx":435C3
      Top             =   6480
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   63
      Left            =   7680
      Picture         =   "pokemurder.frx":43AB8
      Top             =   5880
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   62
      Left            =   7680
      Picture         =   "pokemurder.frx":43F50
      Top             =   4560
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   61
      Left            =   7080
      Picture         =   "pokemurder.frx":443EA
      Top             =   3720
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   60
      Left            =   7560
      Picture         =   "pokemurder.frx":44892
      Top             =   3000
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   59
      Left            =   7080
      Picture         =   "pokemurder.frx":44D13
      Top             =   2280
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   58
      Left            =   9000
      Picture         =   "pokemurder.frx":451EC
      Top             =   5520
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   57
      Left            =   9000
      Picture         =   "pokemurder.frx":4568B
      Top             =   6360
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   56
      Left            =   7920
      Picture         =   "pokemurder.frx":45B12
      Top             =   6720
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   55
      Left            =   8880
      Picture         =   "pokemurder.frx":45FB1
      Top             =   6960
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   54
      Left            =   4800
      Picture         =   "pokemurder.frx":4642C
      Top             =   7080
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   53
      Left            =   5640
      Picture         =   "pokemurder.frx":46869
      Top             =   7080
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   52
      Left            =   7920
      Picture         =   "pokemurder.frx":46CEC
      Top             =   7560
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   51
      Left            =   3720
      Picture         =   "pokemurder.frx":4715D
      Top             =   7200
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   50
      Left            =   3000
      Picture         =   "pokemurder.frx":475FF
      Top             =   7080
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   49
      Left            =   5160
      Picture         =   "pokemurder.frx":47A2C
      Top             =   7800
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   48
      Left            =   4200
      Picture         =   "pokemurder.frx":47ED1
      Top             =   8040
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   47
      Left            =   3360
      Picture         =   "pokemurder.frx":4838E
      Top             =   8040
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   46
      Left            =   840
      Picture         =   "pokemurder.frx":4883D
      Top             =   7320
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   45
      Left            =   1920
      Picture         =   "pokemurder.frx":48CBA
      Top             =   7320
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   44
      Left            =   1200
      Picture         =   "pokemurder.frx":49146
      Top             =   8160
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   43
      Left            =   2520
      Picture         =   "pokemurder.frx":495BF
      Top             =   8040
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   42
      Left            =   2280
      Picture         =   "pokemurder.frx":49A05
      Top             =   6600
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   41
      Left            =   11040
      Picture         =   "pokemurder.frx":49E92
      Top             =   6120
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   40
      Left            =   9840
      Picture         =   "pokemurder.frx":4A2F5
      Top             =   7200
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   39
      Left            =   10680
      Picture         =   "pokemurder.frx":4A785
      Top             =   7440
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   38
      Left            =   11160
      Picture         =   "pokemurder.frx":4ABCC
      Top             =   8040
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   37
      Left            =   10200
      Picture         =   "pokemurder.frx":4B04A
      Top             =   8040
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   36
      Left            =   9480
      Picture         =   "pokemurder.frx":4B4BE
      Top             =   7800
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   35
      Left            =   8760
      Picture         =   "pokemurder.frx":4B927
      Top             =   8040
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   34
      Left            =   7320
      Picture         =   "pokemurder.frx":4BDB6
      Top             =   7440
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   33
      Left            =   6000
      Picture         =   "pokemurder.frx":4C282
      Top             =   8160
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   32
      Left            =   7440
      Picture         =   "pokemurder.frx":4C70F
      Top             =   8040
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   31
      Left            =   6720
      Picture         =   "pokemurder.frx":4CB53
      Top             =   7200
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   30
      Left            =   11160
      Picture         =   "pokemurder.frx":4CFF9
      Top             =   6840
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   29
      Left            =   11280
      Picture         =   "pokemurder.frx":4D45C
      Top             =   5400
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   28
      Left            =   9600
      Picture         =   "pokemurder.frx":4D88E
      Top             =   4920
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   27
      Left            =   11040
      Picture         =   "pokemurder.frx":4DD6A
      Top             =   3600
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   26
      Left            =   6000
      Picture         =   "pokemurder.frx":4E1E5
      Top             =   2760
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   25
      Left            =   7560
      Picture         =   "pokemurder.frx":4E689
      Top             =   5280
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   24
      Left            =   8640
      Picture         =   "pokemurder.frx":4EAFA
      Top             =   2280
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   23
      Left            =   11280
      Picture         =   "pokemurder.frx":4EF95
      Top             =   600
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   22
      Left            =   10440
      Picture         =   "pokemurder.frx":4F407
      Top             =   600
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   21
      Left            =   9000
      Picture         =   "pokemurder.frx":4F8BC
      Top             =   720
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   20
      Left            =   9120
      Picture         =   "pokemurder.frx":4FD4C
      Top             =   1440
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   19
      Left            =   10080
      Picture         =   "pokemurder.frx":50205
      Top             =   1440
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   18
      Left            =   7680
      Picture         =   "pokemurder.frx":50647
      Top             =   720
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   17
      Left            =   7080
      Picture         =   "pokemurder.frx":50B17
      Top             =   1440
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   16
      Left            =   6840
      Picture         =   "pokemurder.frx":50FBB
      Top             =   480
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   15
      Left            =   5040
      Picture         =   "pokemurder.frx":5140B
      Top             =   2160
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   14
      Left            =   6240
      Picture         =   "pokemurder.frx":518D6
      Top             =   1080
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   13
      Left            =   5520
      Picture         =   "pokemurder.frx":51D09
      Top             =   600
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   12
      Left            =   240
      Picture         =   "pokemurder.frx":52116
      Top             =   600
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   11
      Left            =   4320
      Picture         =   "pokemurder.frx":525C9
      Top             =   2520
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   10
      Left            =   3720
      Picture         =   "pokemurder.frx":52A04
      Top             =   3240
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   9
      Left            =   2400
      Picture         =   "pokemurder.frx":52E3A
      Top             =   2040
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   8
      Left            =   2880
      Picture         =   "pokemurder.frx":5334E
      Top             =   240
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   7
      Left            =   1320
      Picture         =   "pokemurder.frx":5380D
      Top             =   4800
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   6
      Left            =   2880
      Picture         =   "pokemurder.frx":53C72
      Top             =   5400
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   5
      Left            =   3960
      Picture         =   "pokemurder.frx":54174
      Top             =   6000
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   4
      Left            =   240
      Picture         =   "pokemurder.frx":545F1
      Top             =   4200
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   3
      Left            =   4920
      Picture         =   "pokemurder.frx":54A78
      Top             =   6600
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   2
      Left            =   5640
      Picture         =   "pokemurder.frx":54F6D
      Top             =   3240
      Width           =   480
   End
   Begin VB.Image Image1 
      Height          =   480
      Index           =   1
      Left            =   1080
      Picture         =   "pokemurder.frx":5540E
      Top             =   1560
      Width           =   480
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Declare Function GetKeyState Lib "user32" (ByVal nVirtKey As Long) As Integer
Dim blowup As Integer, killed As Integer, ammo, score, points As Integer, time1, time2 As String, com As Boolean



Private Sub Form_Click()
On Error Resume Next
Dim rc As Integer
If ammo = 0 Then Exit Sub
rc = sndPlaySound(App.Path & "\Gunfire.WAV", SND_ASYNC)
ammo = ammo - 1
Label1(0).Caption = " Ammo: " & ammo
Label1(1).Caption = " Score: " & score
Label1(2).Caption = " Pokemon: " & (151 - killed)
If ammo = 0 Then Call Finish
End Sub

Private Sub Form_KeyPress(KeyAscii As Integer)
On Error Resume Next
If GetKeyState(vbKeyEscape) < -5 Then
If MsgBox("Are you sure you want to exit?", vbYesNo, "PokeMurder ") = vbYes Then End
End If
End Sub

Private Sub Form_Load()
On Error Resume Next
Dim counter1 As Integer
For counter1 = 1 To 151
left1(counter1) = Image1(counter1).Left
top1(counter1) = Image1(counter1).Top
Next counter1
lr = 0
com = False
points = 5
ud = 0
blowup = 0
score = 0
ammo = 250
time1 = "5"
time2 = "00"
killed = 0
Label1(0).Caption = " Ammo: " & ammo
Label1(1).Caption = " Score: " & score
Label1(2).Caption = " Pokemon: " & (151 - killed)
topscore = 0
Dim filename, dat, counter, ans(2)
filename = App.Path & "\score.dat"
Open filename For Input As #1
For counter = 1 To 2
Line Input #1, dat
Module1.decrypt (dat)
ans(counter) = out
Next counter
Label2.Caption = ans(1) & Chr(vbKeyReturn) & ans(2)
topscore = ans(2)
Close #1
If MsgBox("Are You ready?", vbYesNo, "PokeMurder") = vbNo Then End
End Sub

Private Sub Image1_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
If com = True Then Exit Sub
If ammo = 0 Then Exit Sub
rc = sndPlaySound(App.Path & "\Gunfire.WAV", SND_ASYNC)
ammo = ammo - 1
Label1(0).Caption = " Ammo: " & ammo
If Index = 151 And killed < 151 Then Exit Sub
Image1(Index).Visible = False
Image2(4).Left = Image1(Index).Left - 16
Image2(4).Top = Image1(Index).Top - 16
Timer1.Enabled = True
killed = killed + 1
score = score + points
Label1(1).Caption = " Score: " & score
Label1(2).Caption = " Pokemon: " & (151 - killed)
If killed = 151 Then Call Finish
If ammo = 0 Then Call Finish
End Sub


Private Sub Image4_Click(Index As Integer)
On Error Resume Next
If ammo = 0 Then Exit Sub
rc = sndPlaySound(App.Path & "\Gunfire.WAV", SND_ASYNC)
ammo = ammo - 1
Label1(0).Caption = " Ammo: " & ammo
Label1(1).Caption = " Score: " & score
Label1(2).Caption = " Pokemon: " & (151 - killed)
If ammo = 0 Then Call Finish
End Sub

Private Sub Label1_Click(Index As Integer)
On Error Resume Next
If ammo = 0 Then Exit Sub
rc = sndPlaySound(App.Path & "\Gunfire.WAV", SND_ASYNC)
ammo = ammo - 1
Label1(0).Caption = " Ammo: " & ammo
Label1(1).Caption = " Score: " & score
Label1(2).Caption = " Pokemon: " & (151 - killed)
If ammo = 0 Then Call Finish
End Sub

Private Sub Label2_Click()
On Error Resume Next
Dim rc As Integer
If ammo = 0 Then Exit Sub
rc = sndPlaySound(App.Path & "\Gunfire.WAV", SND_ASYNC)
ammo = ammo - 1
Label1(0).Caption = " Ammo: " & ammo
Label1(1).Caption = " Score: " & score
Label1(2).Caption = " Pokemon: " & (151 - killed)
If ammo = 0 Then Call Finish
End Sub

Private Sub Timer1_Timer()
On Error Resume Next
If blowup = 5 Then Timer1.Enabled = False: blowup = 0: Image2(4).Visible = False: Exit Sub
Image2(4).Visible = True
Image2(4).Picture = Image2(blowup).Picture
blowup = blowup + 1
End Sub

Private Sub Timer2_Timer()
On Error Resume Next
Randomize
Dim counter, no, no2, dist
dist = (4 * (Int((killed / 10) + 1))) + 1
points = ((dist - 1) / 4) * 5
For counter = 1 To 150
DoEvents
no = Int(Rnd * dist) - ((dist - 1) / 2)
no2 = Int(Rnd * dist) - ((dist - 1) / 2)
Image1(counter).Left = Image1(counter).Left + no
Image1(counter).Top = Image1(counter).Top + no2
If Image1(counter).Left < 5 Then Image1(counter).Left = 5
If Image1(counter).Top < 5 Then Image1(counter).Top = 5
If Image1(counter).Left > 760 Then Image1(counter).Left = 760
If Image1(counter).Top > 560 Then Image1(counter).Top = 560
Next counter
dist = dist * 2
no = Int(Rnd * dist) - ((dist / 2) - 1)
no2 = Int(Rnd * dist) - ((dist / 2) - 1)
Image1(counter).Left = Image1(counter).Left + no
Image1(counter).Top = Image1(counter).Top + no2
If Image1(counter).Left < 5 Then Image1(counter).Left = 5
If Image1(counter).Top < 5 Then Image1(counter).Top = 5
If Image1(counter).Left > 760 Then Image1(counter).Left = 760
If Image1(counter).Top > 560 Then Image1(counter).Top = 560
End Sub

Private Sub Timer3_Timer()
On Error Resume Next
If time2 = 0 And time1 = 0 Then Call Finish: Exit Sub
If time2 = 0 Then time2 = "60": time1 = time1 - 1
time2 = time2 - 1
If time2 < 10 Then time2 = "0" & time2
Label1(3).Caption = " Time: " & time1 & ":" & time2
End Sub


Function restart()
On Error Resume Next
Dim counter As Integer
lr = 0
com = False
points = 5
ud = 0
blowup = 0
score = 0
ammo = 250
time1 = "5"
time2 = "00"
killed = 0
Label1(0).Caption = " Ammo: " & ammo
Label1(1).Caption = " Score: " & score
Label1(2).Caption = " Pokemon: " & (151 - killed)
topscore = 0
Dim filename, dat, ans(2)
filename = App.Path & "\score.dat"
Open filename For Input As #1
For counter = 1 To 2
Line Input #1, dat
Module1.decrypt (dat)
ans(counter) = out
Next counter
Label2.Caption = ans(1) & Chr(vbKeyReturn) & ans(2)
topscore = ans(2)
Close #1
For counter = 1 To 151
Image1(counter).Visible = True
Image1(counter).Left = left1(counter)
Image1(counter).Top = top1(counter)
Next counter
Timer2.Enabled = True
Timer3.Enabled = True
End Function

Function Finish()
On Error Resume Next
Dim points, pokemon, ammoa, time, extra, counter, PauseTime, Start, TotalTime, scorename
pokemon = killed
points = score
ammoa = ammo
time = (time1 * 60) + time2
Timer2.Enabled = False
Timer3.Enabled = False
com = True
If pokemon = 151 Then
extra = (50 * time) + (50 * ammoa)
extra = extra + 5000
GoTo final
End If

If pokemon < 26 Then
extra = 0
GoTo final
End If

If pokemon < 51 And pokemon > 25 Then
extra = (1 * time) + (1 * ammoa)
GoTo final
End If

If pokemon < 76 And pokemon > 50 Then
extra = (5 * time) + (5 * ammoa)
GoTo final
End If

If pokemon < 101 And pokemon > 75 Then
extra = (10 * time) + (10 * ammoa)
GoTo final
End If

If pokemon < 126 And pokemon > 100 Then
extra = (15 * time) + (15 * ammoa)
GoTo final
End If

If pokemon < 151 And pokemon > 125 Then
extra = (20 * time) + (20 * ammoa)
GoTo final
End If

final:
For counter = 1 To extra
DoEvents
score = score + 1
Label1(1).Caption = " Score: " & score
    PauseTime = 0.001
    Start = Timer
    Do While Timer < Start + PauseTime
        DoEvents
    Loop
Next counter
myscore = score
If myscore > topscore Then
scorename = InputBox("You have a new high score" & Chr(vbKeyReturn) & "Please enter your name", "PokeMurder")
Module1.topscores (scorename)
End If
If MsgBox("Play Again?", vbYesNo, "PokeMurder") = vbYes Then
Call restart
Exit Function
End If
End
End Function







