import { Component, OnInit } from '@angular/core'
import { BuildingService } from 'src/app/services/building.service'
import { RoomService } from '../../../services/room.service'
import { FloorAndBuildingDTO, FloorService } from '../../../services/floor.service'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { CreatedRoomDTO } from 'src/app/dto/CreatedRoomDTO'

@Component({
    selector: 'app-list-rooms',
    templateUrl: './list-rooms.component.html',
    styleUrls: ['./list-rooms.component.css'],
})
export class ListRoomsComponent implements OnInit {
    selectedBuilding: string
    selectedFloor: string
    rooms: CreatedRoomDTO[]
    buildings: BuildingDTO[]
    floors: FloorAndBuildingDTO[]

    constructor(
        private buildingService: BuildingService,
        private floorService: FloorService,
        private roomService: RoomService,
    ) {
        this.rooms = []
        this.buildings = []
        this.selectedBuilding = ''
        this.floors = []
        this.selectedFloor = ''
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list
        })
    }

    listFloors(): void {
        if (this.selectedBuilding.length !== 0) {
            this.floorService
                .getFloors(this.selectedBuilding)
                .subscribe((list: FloorAndBuildingDTO[]) => {
                    this.floors = list
                })
        }
    }

    getRooms() {
        this.rooms = []
        this.roomService
            .getRooms(this.selectedBuilding, this.selectedFloor)
            .subscribe((list: CreatedRoomDTO[]) => {
                this.rooms = list
            })
    }
}
