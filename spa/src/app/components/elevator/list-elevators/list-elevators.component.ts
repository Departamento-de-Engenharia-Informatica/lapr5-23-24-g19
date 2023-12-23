import { Component, OnInit } from '@angular/core'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import { ElevatorService } from 'src/app/services/elevator.service'
import { CreatedElevatorDTO } from 'src/app/dto/CreatedElevatorDTO'

@Component({
    selector: 'app-list-elevators',
    templateUrl: './list-elevators.component.html',
    styleUrls: ['./list-elevators.component.css'],
})
export class ListElevatorsComponent implements OnInit {
    selectedBuilding: string
    elevators: CreatedElevatorDTO[]
    allElevators: CreatedElevatorDTO[]
    buildings: BuildingDTO[]

    constructor(
        private buildingService: BuildingService,
        private elevatorService: ElevatorService,
    ) {
        this.elevators = []
        this.allElevators = []
        this.buildings = []
        this.selectedBuilding = ''
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list
        })
    }

    getElevators() {
        this.elevators = []
        this.elevatorService.getElevators(this.selectedBuilding).subscribe(
            (list: CreatedElevatorDTO[]) => {
                this.allElevators = list
                this.elevators = this.allElevators
            },
            (error) => {
                alert(error.error)
                this.allElevators = []
                this.elevators = this.allElevators
            },
        )
    }

    filter(event: Event) {
        const prop = (event.target as HTMLInputElement).value.trim().toLowerCase()
        if (prop.length === 0) {
            this.elevators = this.allElevators
        } else {
            this.elevators = this.allElevators.filter(
                (b) =>
                    b.identifier.toString().includes(prop) ||
                    (prop.length > 2 && b.description?.toLowerCase().includes(prop)),
            )
        }
    }
}
