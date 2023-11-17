import {Component, OnInit} from '@angular/core';
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { BuildingDTO, BuildingService } from 'src/app/services/building.service';
import {RoomDTO, RoomService} from 'src/app/services/room.service';

@Component({
  selector: 'app-create-room',
  templateUrl: './create-room.component.html',
  styleUrls: ['./create-room.component.css']
})
export class CreateRoomComponent implements OnInit{

    selectedBuilding: string = '';
    createdRoom = null as unknown as RoomDTO;
    createRoomForm: FormGroup = null as unknown as FormGroup;

    buildings: BuildingDTO[] = [];
    floors: FloorAndBuildingDTO[] = [];

    constructor(
        private formBuilder: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,
        private roomService: RoomService,
    ) {
        this.createRoomForm = this.formBuilder.group({
            name:['',[Validators.required]],
            buildingCode: [null, [Validators.required]],
            floorNumber: [null, [Validators.min(0), Validators.required]],
            description: ['', Validators.required],
            category: ['', Validators.required],
            dimensions: this.formBuilder.group({
                length: [null, [Validators.min(0), Validators.required]],
                width: [null, [Validators.min(0), Validators.required]]
            }),
            positions: this.formBuilder.group({
                x: [null, [Validators.required]],
                y: [null, [Validators.required]]
            })
        });
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list;
        });
    }

    listFloors(): void {
        if (this.selectedBuilding.length !== 0) {
            this.floorService
                .getFloors(this.selectedBuilding)
                .subscribe((list: FloorAndBuildingDTO[]) => {
                    this.floors = list;
                });
        }
    }

    onSubmit(): void {
        const dto: RoomDTO = {
            name: this.createRoomForm.value.name,
            buildingCode: this.createRoomForm.value.buildingCode,
            floorNumber: this.createRoomForm.value.floorNumber,
            description: this.createRoomForm.value.description,
            category: this.createRoomForm.value.category,
            dimensions: {
                length: this.createRoomForm.value.dimensions.length,
                width: this.createRoomForm.value.dimensions.width
            },
            positions: {
                x: this.createRoomForm.value.positions.x,
                y: this.createRoomForm.value.positions.y
            }
        };

        this.roomService
            .createRoom(dto)
            .subscribe((room: RoomDTO) => {
                this.createdRoom = room;
            });
    }
}
