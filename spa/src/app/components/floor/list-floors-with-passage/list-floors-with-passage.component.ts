import { Component, OnInit } from '@angular/core'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import { FloorPassageDTO } from '../../../dto/FloorPassageDTO'
import { FloorAndBuildingDTO, FloorService } from '../../../services/floor.service'

@Component({
    selector: 'app-list-floors-with-passage',
    templateUrl: './list-floors-with-passage.component.html',
    styleUrls: ['./list-floors-with-passage.component.css'],
})
export class ListFloorsWithPassageComponent implements OnInit {
    selectedBuilding: string
    buildings: BuildingDTO[]

    floorPassages: FloorPassageDTO[]

    allFloorsPassages: FloorPassageDTO[]

    constructor(
        private buildingService: BuildingService,
        private floorService: FloorService,
    ) {
        this.buildings = []
        this.selectedBuilding = ''
        this.floorPassages = []
        this.allFloorsPassages = []
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list
        })
    }

    getFloorsWithPassage() {
        this.floorService.getFloorsWithPassage(this.selectedBuilding).subscribe(
            (list: FloorPassageDTO[]) => {
                this.allFloorsPassages = list

                if (this.allFloorsPassages.length === 0) alert('Floors without passages')

                this.floorPassages = this.allFloorsPassages
            },
            (error) => {
                alert(error.error)
                this.allFloorsPassages = []
                this.floorPassages = this.allFloorsPassages
            },
        )
    }

    filter(event: Event) {
        const prop = (event.target as HTMLInputElement).value.trim().toLowerCase()
        if (prop.length === 0) {
            this.floorPassages = this.allFloorsPassages
        } else {
            this.floorPassages = this.allFloorsPassages.filter(
                (b) =>
                    b.floor.floorNumber.toString().includes(prop) ||
                    (prop.length > 2 &&
                        b.floor.description?.toLowerCase().includes(prop)),
            )
        }
    }
}
