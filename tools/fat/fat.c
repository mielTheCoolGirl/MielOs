#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef uint8_t bool;
#define true 1
#define false 0

typedef struct BootSector {
    uint8_t BootJumpInstruction[3];
    uint8_t OemIdentifier[8];
    uint16_t BytesPerSector;
    uint8_t SectorsPerCluster;
    uint16_t ReservedSectors;
    uint8_t FatCount;
    uint16_t DirEntryCount;
    uint16_t TotalSectors;
    uint8_t MediaDescriptorType;
    uint16_t SectorsPerFat;
    uint16_t SectorsPerTrack;
    uint16_t Heads;
    uint32_t HiddenSectors;
    uint32_t LargeSectorCount;

    // extended boot record
    uint8_t DriveNumber;
    uint8_t _Reserved;
    uint8_t Signature;
    uint32_t VolumeId;        // serial num (value doesnt matter)
    uint8_t VolumeLabel[11];  // 11 bytes padding with spaces
    uint8_t SysId[8];
} __attribute__((packed)) BootSector;

typedef struct DirectoryEntry {
    uint8_t name[11];
    uint8_t attributes;
    uint8_t reserved;
    uint8_t createTimeTenths;
    uint16_t createdTime;
    uint16_t createdDate;
    uint16_t lastAccessDate;
    uint16_t modifiedTime;
    uint16_t modifiedDate;
    uint16_t firstClusterHigh;
    uint16_t firstClusterLow;
    uint32_t size;
} __attribute__((packed)) DirectoryEntry;


BootSector global_bootSect;
uint8_t* global_fat = NULL;
DirectoryEntry* global_root_dir = NULL; // should point to the beginning of the dir entry
uint32_t global_root_dir_end;


bool readBootSector(FILE* disk) {
    return fread(&global_bootSect, sizeof(global_bootSect), 1, disk) > 0;
}

bool readSectors(FILE* disk, uint32_t lba, uint32_t count, void* bufferOut) {
    // function checks if its possible to read the wanted sectors, can only read FULL sectors
    bool status = true;
    status = status && (fseek(disk, lba * global_bootSect.BytesPerSector, SEEK_SET) == 0);
    status = status && (fread(bufferOut, global_bootSect.BytesPerSector, count, disk) == count);
    return status;
}

bool readFat(FILE* disk) {
    // trying to read fat (fat is after the boot sector's reserved section)
    global_fat = (uint8_t*)malloc(global_bootSect.SectorsPerFat * global_bootSect.BytesPerSector);
    return readSectors(disk, global_bootSect.ReservedSectors, global_bootSect.SectorsPerFat, global_fat);
}

bool readRootDir(FILE* disk) {
    uint32_t lba = global_bootSect.ReservedSectors + global_bootSect.SectorsPerFat * global_bootSect.FatCount;
    uint32_t size = sizeof(DirectoryEntry) * global_bootSect.DirEntryCount;
    uint32_t sectors = (size / global_bootSect.BytesPerSector); // find how many sectors 2 read

    // rounding up the sectors amount
    if (size % global_bootSect.BytesPerSector > 0) {
        sectors++;
    }

    global_root_dir_end = lba + sectors;
    global_root_dir = (DirectoryEntry*)(malloc(sectors * global_bootSect.BytesPerSector));
    return readSectors(disk, lba, sectors, global_root_dir);
}

DirectoryEntry* findFile(const char* name) {
    for (uint32_t i = 0; i < global_bootSect.DirEntryCount; i++) {
        if (memcmp(name, global_root_dir[i].name, 11) == 0) {
            return &global_root_dir[i];
        }
    }
    return NULL;
}

bool readFile(DirectoryEntry* fileEntry, FILE* disk, uint8_t* outputBuffer) {
    // getting current cluster from the file entry
    uint16_t currCluster = fileEntry->firstClusterLow;
    uint16_t endOfClusterChain = 0x0FF8;
    bool readStatus = true;

    do {
        uint32_t lba = global_root_dir_end + (currCluster - 2) * global_bootSect.SectorsPerCluster; 
        // minus 2 since the first two clusters are reserved

        readStatus = readStatus && readSectors(disk, lba, global_bootSect.SectorsPerCluster, outputBuffer);
        outputBuffer += global_bootSect.SectorsPerCluster * global_bootSect.BytesPerSector;
        // advancing the buffer by X sectors (in bytes)

        uint32_t fatIndex = currCluster * 3 / 2;

        // if its an even number for cluster then take the bottom 12 bits and apply a bitmask to remove the top ones
        // if its odd then we will take the top 12 bits by shifting in 4 bits
        if (currCluster % 2 == 0) {
            currCluster = (*(uint16_t*)(global_fat + fatIndex)) & 0x0FFF;
        } else {
            currCluster = (*(uint16_t*)(global_fat + fatIndex)) >> 4;
        }

    } while (readStatus && currCluster < endOfClusterChain);

    return readStatus;
}


int main(int argc, char** argv) {
    if (argc < 3) {
        printf("Synthax: %s <disk image> <file name>\n", argv[0]);
        return -1;
    }

    FILE* disk = fopen(argv[1], "rb");
    if (!disk) {
        printf("Cant open disk image!");
        return -1;
    }

    if (!readBootSector(disk)) {
        fprintf(stderr, "Couldn't read boot sector!\n");
        return -2;
    }

    if (!readFat(disk)) {
        fprintf(stderr, "Couldn't read FAT!\n");
        free(global_fat);
        return -3;
    }

    if (!(readRootDir(disk))) {
        fprintf(stderr, "Couldn't read root directory!\n");
        free(global_fat);
        free(global_root_dir);
        return -4;
    }

    DirectoryEntry* fileEntry = findFile(argv[2]);
    if (!fileEntry) {
        fprintf(stderr, "Couldn't find file: %s\n", argv[2]);
        free(global_fat);
        free(global_root_dir);
        return -5;
    }

    uint8_t* buffer = (uint8_t*)(malloc(fileEntry->size + global_bootSect.BytesPerSector));
    if (!readFile(fileEntry, disk, buffer)) {
        fprintf(stderr, "Couldn't read file: %s\n", argv[2]);
        free(global_fat);
        free(global_root_dir);
        free(buffer);
        return -6;
    }

    for (int i = 0; i < fileEntry->size; i++) {
        if (isprint(buffer[i])) {
            fputc(buffer[i], stdout);
        } else {
            printf("<%02x>", buffer[i]);
        }
    }
    printf("\n");

    free(global_fat);
    free(global_root_dir);
    free(buffer);

    return 0;
}
