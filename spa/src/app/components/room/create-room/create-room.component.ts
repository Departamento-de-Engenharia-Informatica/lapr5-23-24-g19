import { Component, OnInit } from '@angular/core'
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { BuildingService } from 'src/app/services/building.service'
import { RoomService } from 'src/app/services/room.service'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { CreatedRoomDTO } from 'src/app/dto/CreatedRoomDTO'
import { RoomDTO } from 'src/app/dto/RoomDTO'

enum CategoryType {
    GABINETE = 'GABINETE',
    ANFITEATRO = 'ANFITEATRO',
    LABORATORIO = 'LABORATORIO',
    OUTRO = 'OUTRO',
}

@Component({
    selector: 'app-create-room',
    templateUrl: './create-room.component.html',
    styleUrls: ['./create-room.component.css'],
})
export class CreateRoomComponent implements OnInit {
    selectedBuilding: string = ''
    selectedFloor: string = ''
    createdRoom = null as unknown as CreatedRoomDTO
    createRoomForm: FormGroup = null as unknown as FormGroup

    categoryTypes = Object.values(CategoryType)
    buildings: BuildingDTO[] = []
    floors: FloorAndBuildingDTO[] = []

    constructor(
        private formBuilder: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,
        private roomService: RoomService,
    ) {
        this.createRoomForm = this.formBuilder.group({
            name: ['', [Validators.required]],
            buildingCode: [null, [Validators.required]],
            floorNumber: [null, [Validators.min(0), Validators.required]],
            description: ['', Validators.required],
            category: ['', Validators.required],
            dimensions: this.formBuilder.group({
                length: [null, [Validators.min(1), Validators.required]],
                width: [null, [Validators.min(1), Validators.required]],
            }),
            positions: this.formBuilder.group({
                x: [null, [Validators.required]],
                y: [null, [Validators.required]],
            }),
        })
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list
        })
    }

    listFloors(): void {
        if (this.selectedBuilding.length !== 0) {
            this.floorService.getFloors(this.selectedBuilding).subscribe(
                (list: FloorAndBuildingDTO[]) => {
                    this.floors = list
                },
                (error) => {
                    alert(error.error)
                    this.floors = []
                },
            )
        }
    }

    onSubmit(): void {
        const dto: RoomDTO = {
            name: this.createRoomForm.value.name,
            description: this.createRoomForm.value.description,
            category: this.createRoomForm.value.category,
            dimensions: {
                length: this.createRoomForm.value.dimensions.length,
                width: this.createRoomForm.value.dimensions.width,
            },
            positions: {
                x: this.createRoomForm.value.positions.x,
                y: this.createRoomForm.value.positions.y,
            },
        }

        const buildingCode = this.createRoomForm.value.buildingCode
        const floorNumber = this.createRoomForm.value.floorNumber

        this.roomService.createRoom(buildingCode, floorNumber, dto).subscribe(
            (room: CreatedRoomDTO) => {
                let alertMessage = 'Room created successfully!\n'
                alert(alertMessage)

                this.createdRoom = room
                this.createRoomForm.reset()
            },
            (error) => {
                alert(error.error)
                this.createRoomForm.reset()
            },
        )
    }
}
