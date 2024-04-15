#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <time.h>
#include <windows.h>
#include "fake6502.c"

typedef unsigned short word;

void LoadMEM(char*, char*, word, word);
void UpdateDisplay();
void SaveRAM();
void SimulateMech();
void HandleKeyboard();
byte *MEM;
FILE *fp, *fp2;
char charcode[16] = {'0','1','2','3','4','5','6','7','8','9','-','=','E','r',' ','F'};
int keypad;
float emu_time;
int adv_res, svc_mode, batt_low, opto_mag, opto_home, mag_pos, outer_cam, inner_cam;
int page, xfer_pos, most_least, rec_vid;
int o_mute,o_detent,o_magazine,o_transfer;
int o_turntable,o_toggle,o_playcnt,o_coincnt;
clock_t basetime, ref_time;
int quitting, filesize;

int main() {
  emu_time=0; svc_mode=0; mag_pos=1; opto_mag=0; outer_cam=1; inner_cam=0; xfer_pos=0;
  system("cls");
  printf("Rowem v0.2: A Rowe CCC Emulator by DarrenF\n\n");

  MEM = malloc(0x10000);
  
  fp=fopen("rom.bin", "rb");
  if(!fp) {
    printf("Error opening rom.bin");
    exit(1);
  }
  fseek(fp, 0, SEEK_END);
  filesize = ftell(fp);
  fclose(fp);
  if (filesize == 0x2000)
    LoadMEM("rom.bin", MEM, 0xe000, 0x2000);
  if (filesize == 0x4000)
    LoadMEM("rom.bin", MEM, 0xc000, 0x4000);

   printf("Simulate loss of battery power (reset all RAM)? ");
  if (getch()!='y') {
    printf("n\n\n");
    LoadMEM("ram.bin", MEM, 0x0000, 0x0800);
  } else { printf("y\n\n"); }

  printf("Hold ADVANCE and RESET? ");
  if (getch()=='y') { 
    adv_res = 1;
    printf("y\n"); 
  } else { printf("n\n"); }

  system("cls");
  printf("\033[?25lRowem: A Rowe CCC Emulator by DarrenF\n\n");

//  fp2=fopen("rowe_events.txt", "w");
//  fprintf(fp2, "Event log:\n");

  reset6502();
  while (1) {
    Sleep(7);				// this throttles speed OK on my laptop
    exec6502(27000);			// 900000 cycles/s * 0.030s = 27000 cycles
    emu_time+=0.03;			// ~30ms time increments
    SimulateMech();			// switches and optos on record mechanism
    UpdateDisplay();			//
    HandleKeyboard();			//
    if (quitting == 1) break;		//
  }

//  fclose(fp2);

  printf("Saving RAM...\033[?25h");
    SaveRAM();
  printf("done.\n");
  return(0);
}

void write6502(uint16_t Addr, uint8_t Value) {
  if (Addr < 0x0800) {
    MEM[Addr] = Value;
    return;
  }
  if (Addr == 0x2000) {
    MEM[Addr] = (MEM[Addr]|(Value & 0x01));
//      MEM[Addr] = Value;
    return;
  }
  if (Addr == 0x2001) {
    MEM[Addr] = Value;
    return;
  }
  if (Addr == 0x2002) {
    MEM[Addr] = Value;
    if (Value & 0x01) o_mute=0; else o_mute=1;
    if (Value & 0x02) o_detent=1; else o_detent=0;
    if (Value & 0x04) o_magazine=1; else o_magazine=0;
    if (Value & 0x08) o_transfer=1; else o_transfer=0; 
    if (Value & 0x10) o_turntable=1; else o_turntable=0;
    if (Value & 0x20) o_toggle=1; else o_toggle=0;
    if (Value & 0x40) o_playcnt=1; else o_playcnt=0;
    if (Value & 0x80) o_coincnt=1; else o_coincnt=0;
    return;
  }
  if (Addr == 0x2003) {
    MEM[Addr] = Value;
    return;
  }
  if (Addr == 0x4000) {

    if (Value & 0x04) {	     // this ties the wallbox
      Value = Value & ~0x10; // output and input,
    }			     // to simulate no 
    else {		     // wallbox connected
      Value = Value | 0x10;  //
    }			     //
    MEM[Addr] = Value;
    return;
  }
  if (Addr == 0x4001) {
    MEM[Addr] = Value;
    return;
  }
  if (Addr == 0x4002) {
    MEM[Addr] = Value;
    return;
  }
  if (Addr == 0x4003) {
    MEM[Addr] = Value;
    return;
  }
}

uint8_t read6502(uint16_t Addr) {
  if(Addr < 0x0800 || Addr >= 0xc000) return MEM[Addr];		// simple job if reading RAM or ROM

  if(Addr == 0x2000) {						// the rest is for memory-mapped IO (PIAs)
    byte val=0xf8;

    if (batt_low==1) {
      val &= ~0x02;		// battery voltage low
    } 
    else val |= 0x02;
 
   if (svc_mode==1) {
      val |= 0x04;		// service mode (active low)
    }
    else val &= ~0x04;

    if (opto_mag == 1) {
      val |= 0x20;		// magazine "index" opto
    }
    else val &= ~0x20;

    if (opto_home == 0) {
      val &= ~0x10;		// magazine "home" opto
    }
    else val |= 0x10;

    if (keypad == 19) {
      val |= 0x80;		// CANCEL button
    }
    else val &= ~0x80;

    if (outer_cam == 1) {
      val &= ~0x08;		// outer cam switch
    }
    else val |= 0x08;

    if (inner_cam == 1) {
      val &= ~0x40;		// inner cam switch
    }
    else val |= 0x40;

    return (val);
  }
  if(Addr == 0x2001) {
    return MEM[Addr];
  }
  if(Addr == 0x2002) {
    return MEM[Addr];
  }
  if(Addr == 0x2003) {
    return MEM[Addr];
  }

  if(Addr == 0x4000) {
    if ((MEM[0x4002] & 0x0e) == 0x0e) {
      if (keypad == 15) {
        MEM[Addr] |= 0x01;		//$1 (active high)
      }
      else MEM[Addr] &= ~0x01; 
    }
    if ((MEM[0x4002] & 0x0e) == 0x00) {
      if (keypad == 14) {
        MEM[Addr] &= ~0x01;		//$0.50 (active low)
      }
      else MEM[Addr] |= 0x01;
    }
    if ((MEM[0x4002] & 0x0e) == 0x0c) {
      if (keypad == 13) {
        MEM[Addr] &= ~0x01;		//$0.25 (active low)
      }
      else MEM[Addr] |= 0x01;
    }
    if ((MEM[0x4002] & 0x0e) == 0x08) {
      if (keypad == 12) {
        MEM[Addr] &= ~0x01;		//$0.10 (active low)
      }
      else MEM[Addr] |= 0x01;
    }
    if ((MEM[0x4002] & 0x0e) == 0x04) {
      if (keypad == 11) {
        MEM[Addr] &= ~0x01;		//$0.05 (active low)
      }
      else MEM[Addr] |= 0x01;
    }

    if ((MEM[0x4002] & 0xc1) == 0xc1) {
      if (adv_res == 1 && emu_time < 1.) {
        MEM[Addr] &= ~0x02;		 //ADVANCE button
      }
      else if (keypad == 20) {
        MEM[Addr] &= ~0x02;		 //ADVANCE button
      }
      else MEM[Addr] |= 0x02;
    }

    if ((MEM[0x4002] & 0xc1) == 0x80) {
      if (adv_res == 1 && emu_time < 1.) {
        MEM[Addr] &= ~0x02; 		// CCC RESET button
      }
      else if (keypad == 21) {
        MEM[Addr] &= ~0x02; 		// CCC RESET button
      }
      else MEM[Addr] |= 0x02;
    }

    if ((MEM[0x4002] & 0xc1) == 0x81) {
      if (most_least == 1) {
        MEM[Addr] &= ~0x02;		// MOST/LEAST switch
      }
      else MEM[Addr] |= 0x02;
    }

    if ((MEM[0x4002] & 0xcf) == 0x4c) { 
      if (keypad == 1) {
        MEM[Addr] &= ~0x02;		// "1" (active low)
      }
      else MEM[Addr] |= 0x02;		
    }
    if ((MEM[0x4002] & 0xcf) == 0x48) {
      if (keypad == 2) {
        MEM[Addr] &= ~0x02;		// "2" (active low)
      }
      else MEM[Addr] |= 0x02; 
    }
    if ((MEM[0x4002] & 0xcf) == 0x44) {
      if (keypad == 3) {
        MEM[Addr] &= ~0x02;		// "3" (active low)
      }
      else MEM[Addr] |= 0x02; 
    }
    if ((MEM[0x4002] & 0xcf) == 0x4f) {
      if (keypad == 4) {
        MEM[Addr] &= ~0x02;		// "4" (active low)
      }
      else MEM[Addr] |= 0x02; 
    }
    if ((MEM[0x4002] & 0xcf) == 0x43) {
      if (keypad == 5) {
        MEM[Addr] &= ~0x02;		// "5" (active low)
      }
      else MEM[Addr] |= 0x02; 
    }
    if ((MEM[0x4002] & 0xcf) == 0x42) {
      if (keypad == 6) {
        MEM[Addr] &= ~0x02;		// "6" (active low)
      }
      else MEM[Addr] |= 0x02; 
    }
    if ((MEM[0x4002] & 0xcf) == 0x46) {
      if (keypad == 7) {
        MEM[Addr] &= ~0x02;		// "7" (active low)
      }
      else MEM[Addr] |= 0x02; 
    }
    if ((MEM[0x4002] & 0xcf) == 0x4a) {
      if (keypad == 8) {
        MEM[Addr] &= ~0x02;		// "8" (active low)
      }
      else MEM[Addr] |= 0x02; 
    }
    if ((MEM[0x4002] & 0xcf) == 0x49) {
      if (keypad == 9) {
        MEM[Addr] &= ~0x02;		// "9" (active low)
      }
      else MEM[Addr] |= 0x02; 
    }
    if ((MEM[0x4002] & 0xcf) == 0x4d) {
      if (keypad == 10 || keypad ==18) {
        MEM[Addr] &= ~0x02;		// "0" (active low)
      }
      else MEM[Addr] |= 0x02; 
    }
    if ((MEM[0x4002] & 0xcf) == 0x41) {
      if (keypad == 16 || keypad == 18) {
        MEM[Addr] &= ~0x02;		// "POPULAR" (active low)
      }
      else MEM[Addr] |= 0x02; 
    }
    if ((MEM[0x4002] & 0xcf) == 0x45) {
      if (keypad == 17) {
        MEM[Addr] &= ~0x02;		// "RESET" (active low)
      }
      else MEM[Addr] |= 0x02; 
    }

    return (MEM[Addr]);
  }

  if(Addr == 0x4001) {
    return MEM[Addr];
  }
  if(Addr == 0x4002) {
    return MEM[Addr];
  }
  if(Addr == 0x4003) {
    return MEM[Addr];
  }
}

void LoadMEM(char *name, char *loc, word pos, word len) {
  fp=fopen(name, "rb");
  if(!fp) {
    printf("Error loading ROM: %s\n",name);
    exit(1);
  }
  fread(&loc[pos], len, 1, fp);
  fclose(fp);
  return;
}

void SaveRAM() {
  int count;
  fp=fopen("ram.bin", "wb");
  for (count=0;count < 0x0800; count++) {
    fwrite(&MEM[count],1,1,fp);
  }
  fclose(fp);
  return;
}

void UpdateDisplay() { 
  int i;
  COORD coordinate;
  coordinate.X = 5;
  coordinate.Y = 2;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);
  SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE), coordinate);
  printf("\033[3;41;30m");
  printf("%c",   charcode[MEM[0x0094]&0x0f]);
  printf("%c",   charcode[MEM[0x0095]&0x0f]);
  printf("%c", charcode[MEM[0x0096]&0x0f]);
  printf("\033[0m  \033[3;41;30m");
  printf("%c",   charcode[MEM[0x0090]&0x0f]);
  printf("%c",   charcode[MEM[0x0091]&0x0f]);
  printf("%c", charcode[MEM[0x0092]&0x0f]);
  printf("\033[0m  \033[3;41;30m");
  printf("%c",   charcode[MEM[0x008c]&0x0f]);
  printf("%c",   charcode[MEM[0x008d]&0x0f]);
  printf("%c", charcode[MEM[0x008e]&0x0f]);
  printf("\033[0m  \033[3;41;30m");
  printf("%c",   charcode[MEM[0x0028]&0x0f]);
  printf("%c",   charcode[MEM[0x0029]&0x0f]);
  printf("%c", charcode[MEM[0x002a]&0x0f]);
  printf("\033[0m  ");
  if (MEM[0x0038]&0x02) printf("\033[3;41;30mTHA\033[0m  "); else printf("\033[0mTHA  ");
  if (MEM[0x0038]&0x01) printf("\033[3;41;30mMAS\033[0m  "); else printf("\033[0mMAS  ");
  printf("\033[0m    \033[3;42;30m");
  printf("%c",   charcode[MEM[0x005e]&0x0f]);
  printf("%c",   charcode[MEM[0x005f]&0x0f]);
  printf("%c",   charcode[MEM[0x0060]&0x0f]);
  printf("%c",   charcode[MEM[0x0061]&0x0f]);
  printf("\033[0m\n\n");
  if (svc_mode == 1) printf("\033[3;43;30mSVC\033[0m  "); else printf("\033[0mSVC  ");
  if (batt_low == 1) printf("\033[3;43;30mBAT\033[0m  "); else printf("\033[0mBAT  ");
  if (most_least == 0) printf("\033[3;43;30mMOS\033[0m/LEA  "); else printf("MOS/\033[3;43;30mLEA\033[0m  ");
  if (rec_vid == 0) printf("\033[3;43;30mREC\033[0m/VID  "); else printf("REC/\033[3;43;30mVID\033[0m  ");
  printf("\n\n");
  if (o_mute == 1) printf("\033[3;43;30mMUT\033[0m  "); else printf("\033[0mMUT  ");
  if (o_detent == 1) printf("\033[3;43;30mDET\033[0m  "); else printf("\033[0mDET  ");
  if (o_magazine == 1) printf("\033[3;43;30mMAG\033[0m  "); else printf("\033[0mMAG  ");
  if (o_transfer == 1) printf("\033[3;43;30mTFR\033[0m  "); else printf("\033[0mTFR  ");
  if (o_turntable == 1) printf("\033[3;43;30mTTM\033[0m  "); else printf("\033[0mTTM  ");
  if (o_toggle == 1) printf("\033[3;43;30mTOG\033[0m  "); else printf("\033[0mTOG  ");
  if (o_playcnt == 1) printf("\033[3;43;30mPCT\033[0m  "); else printf("\033[0mPCT  ");
  if (o_coincnt == 1) printf("\033[3;43;30mCCT\033[0m  "); else printf("\033[0mCCT  ");

  printf("\n\n\033[3;45;30m");
  printf("%01x", page);
  printf(" 00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f\033[0m\n");
  printf("\033[3;45;30m0\033[0m ");
  for (i=0x100*page; i<(0x100*page+0x100); i++) {
    printf("%02x ", MEM[i]);
    if ((i+1) % 16 == 0) {
      printf("\n\033[3;45;30m");
      printf("%1x", ((i+1)/16)&0x0f);
      printf("\033[0m ");
    }
  }
  printf("\nemulated time = %.1f\n", emu_time);
//  printf("real = %d", (clock()-basetime)/CLOCKS_PER_SEC);
//  printf("%03i ",mag_pos);
//  printf("%02i ",xfer_pos);
}

void SimulateMech() {
  if (o_magazine == 1) opto_mag = 1 - opto_mag;	// toggle opto if magazine is spinning

  if (opto_mag == 1 && o_magazine == 1) {	// if magazine is spinning and opto on,
    mag_pos++;					// increment magazine position
    if (mag_pos == 100) mag_pos = 0;		// wrap around
  }
  
  if (mag_pos == 0) opto_home = 1;		// set home opto condition 
    else opto_home = 0;				// appropriately

  if (o_transfer == 1) {			// simulate the transfer mechanism
    xfer_pos++;					// including cam sensors;
    if (xfer_pos == 100) xfer_pos = 0;		//   (this is currently very crude
    if (xfer_pos == 0) outer_cam = 1;		//    and possibly incorrect, but
    if (xfer_pos == 10) outer_cam = 0;		//    it seems to minimally function)
    if (xfer_pos == 50) inner_cam = 1;		//
    if (xfer_pos == 60) inner_cam = 0;		//
  }
}

void HandleKeyboard() {
  char key;
    keypad = 0;
    if ( kbhit() ) {
      key = getch();
      if (key == 'q') quitting = 1;	// quit emulator
      if (key == '!') reset6502();	// emulator CPU reset
      if (key == '1') keypad = 1;
      if (key == '2') keypad = 2;
      if (key == '3') keypad = 3;
      if (key == '4') keypad = 4;
      if (key == '5') keypad = 5;
      if (key == '6') keypad = 6;
      if (key == '7') keypad = 7;
      if (key == '8') keypad = 8;
      if (key == '9') keypad = 9;
      if (key == '0') keypad = 10;	
      if (key == 'c') keypad = 11;	// coin level 1 (nickel)
      if (key == 'v') keypad = 12;	// coin level 2 (dime)
      if (key == 'b') keypad = 13;	// coin level 3 (quarter)
      if (key == 'n') keypad = 14;	// coin level 4 (half-dollar)
      if (key == 'm') keypad = 15;	// coin level 5 (dollar bill)
      if (key == 'p') keypad = 16;	// POPULAR
      if (key == 'r') keypad = 17;	// RESET (keypad button)
      if (key == 'o') keypad = 18;	// 0+POPULAR (used to enter prog mode)
      if (key == 'x') keypad = 19;	// CANCEL 
      if (key == 'a') keypad = 20;	// ADVANCE (CCC button)
      if (key == 'z') keypad = 21;	// RESET (CCC button)
      if (key == 'l')
        most_least = 1 - most_least;	// MOST/LEAST switch toggle
      if (key == 't') 
        rec_vid = 1 - rec_vid;		// RECORD/VIDEO switch toggle
      if (key == 's') { 		
        svc_mode = 1 - svc_mode;	// SERVICE/ON mode switch toggle
        reset6502();
      }
      if (key == '-')
        batt_low = 1 - batt_low;	// toggle battery voltage signal
      if (key == ' ') {
        printf("PAUSED");
        getch();			// pause emulation
        printf("\b\b\b\b\b\b      ");
      }
      if (key == '.') page++;		// memory dump page increment
      if (key == ',') page--;		// memory dump page decrement
      if (page < 0) page = 0;
      if (page > 7) page = 7;
    }
}
