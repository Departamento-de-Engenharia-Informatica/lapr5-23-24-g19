import { Component, Input, OnChanges, OnInit, SimpleChanges } from '@angular/core'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service'

@Component({
    selector: 'app-list-floors',
    templateUrl: './list-floors.component.html',
    styleUrls: ['./list-floors.component.css'],
})
export class ListFloorsComponent implements OnInit {
    selectedBuilding: string
    floors: FloorAndBuildingDTO[]
    allFloors: FloorAndBuildingDTO[]
    buildings: BuildingDTO[]

    constructor(
        private buildingService: BuildingService,
        private floorService: FloorService,
    ) {
        this.floors = []
        this.allFloors = []
        this.buildings = []
        this.selectedBuilding = ''
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe(
            (list: BuildingDTO[]) => {
                this.buildings = list
            },
            (error) => {
                alert(error.error)
                this.buildings = []
            },
        )
    }

    getFloors(buildingCode: string) {
        this.floors = []
        this.floorService.getFloors(buildingCode).subscribe(
            (list: FloorAndBuildingDTO[]) => {
                this.allFloors = list
                this.floors = this.allFloors
            },
            (error) => {
                alert(error.error)
                this.allFloors = []
                this.floors = this.allFloors
            },
        )
    }

    filter(event: Event) {
        const prop = (event.target as HTMLInputElement).value.trim().toLowerCase()
        if (prop.length === 0) {
            this.floors = this.allFloors
        } else {
            this.floors = this.allFloors.filter(
                (b) =>
                    b.floorNumber.toString().includes(prop) ||
                    (prop.length > 2 && b.description?.toLowerCase().includes(prop)),
            )
        }
    }
}
