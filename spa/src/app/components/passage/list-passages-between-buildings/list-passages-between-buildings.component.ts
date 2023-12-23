import { Component, OnInit } from '@angular/core'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import { PassageService } from 'src/app/services/passage.service'
import { PassageDTO } from 'src/app/dto/PassageDTO'

@Component({
    selector: 'app-list-passages-between-buildings',
    templateUrl: './list-passages-between-buildings.component.html',
    styleUrls: ['./list-passages-between-buildings.component.css'],
})
export class ListPassagesBetweenBuildingsComponent implements OnInit {
    selectedBuilding1: string
    selectedBuilding2: string
    passages: PassageDTO[]

    allPassages: PassageDTO[]
    buildings: BuildingDTO[]

    constructor(
        private buildingService: BuildingService,
        private passageService: PassageService,
    ) {
        this.passages = []
        this.allPassages = []
        this.buildings = []
        this.selectedBuilding1 = ''
        this.selectedBuilding2 = ''
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list
        })
    }

    getPassagesBetweenBuildings() {
        this.passages = []
        this.passageService
            .getPassagesBetweenBuildings(this.selectedBuilding1, this.selectedBuilding2)
            .subscribe(
                (list: PassageDTO[]) => {
                    this.allPassages = list
                    this.passages = this.allPassages
                },
                (error) => {
                    alert(error.error)
                    this.allPassages = []
                    this.passages = this.allPassages
                },
            )
    }

    filter(event: Event) {
        const prop = (event.target as HTMLInputElement).value.trim().toLowerCase()
        if (prop.length === 0) {
            this.passages = this.allPassages
        } else {
            this.passages = this.allPassages.filter(
                (b) =>
                    b.floor1.floorNumber.toString().includes(prop) ||
                    b.floor2.floorNumber.toString().includes(prop),
            )
        }
    }
}
