import { Component, OnInit } from '@angular/core';
import {
    BuildingDTO,
    BuildingService,
} from 'src/app/services/building.service';
import {ElevatorDTO, ElevatorService, CreatedElevatorDTO} from 'src/app/services/elevator.service';

@Component({
    selector: 'app-list-elevators',
    templateUrl: './list-elevators.component.html',
    styleUrls: ['./list-elevators.component.css'],
})
export class ListElevatorsComponent implements OnInit {
    selectedBuilding: string;
    elevators: CreatedElevatorDTO[];
    buildings: BuildingDTO[];

    constructor(
        private buildingService: BuildingService,
        private elevatorService: ElevatorService,
    ) {
        this.elevators = [];
        this.buildings = [];
        this.selectedBuilding = '';
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list;
        });
    }

    getElevators() {
        this.elevators = [];
        this.elevatorService
            .getElevators(this.selectedBuilding)
            .subscribe((list: CreatedElevatorDTO[]) => {
                this.elevators = list;
            });
    }
}
