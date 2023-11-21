import { Component, OnInit } from '@angular/core'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import { PassageDTO, PassageService } from 'src/app/services/passage.service'

@Component({
    selector: 'app-list-passages-between-buildings',
    templateUrl: './list-passages-between-buildings.component.html',
    styleUrls: ['./list-passages-between-buildings.component.css'],
})
export class ListPassagesBetweenBuildingsComponent implements OnInit {
    selectedBuilding1: string
    selectedBuilding2: string
    passages: PassageDTO[]
    buildings: BuildingDTO[]

    constructor(
        private buildingService: BuildingService,
        private passageService: PassageService,
    ) {
        this.passages = []
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
            .subscribe((list: PassageDTO[]) => {
                this.passages = list
            })
    }
}
