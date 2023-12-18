import { Component } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { CreateSurveillanceTaskDTO } from 'src/app/dto/CreateSurveillanceTaskDTO'
import { BuildingService } from 'src/app/services/building.service'
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service'
import { TaskService } from 'src/app/services/task.service'

@Component({
    selector: 'app-create-task-surveillance',
    templateUrl: './create-task-surveillance.component.html',
    styleUrls: ['./create-task-surveillance.component.css'],
})
export class CreateTaskSurveillanceComponent {
    selectedBuilding: string = ''
    selectedFloor: number = null as unknown as number

    createSurveillanceForm: FormGroup = null as unknown as FormGroup

    buildings: BuildingDTO[] = []
    floors: FloorAndBuildingDTO[] = []

    constructor(
        private formBuilder: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,
        private TaskService: TaskService,
    ) {
        this.createSurveillanceForm = this.formBuilder.group({
            email: [null, [Validators.required]],

            buildingCode: [null, [Validators.required]],
            floorNumber: [null, [Validators.required]],

            contactName: [null, [Validators.required]],
            contactPhone: [null, [Validators.required]],
        })
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

    onSubmit(): void {
        const dto: CreateSurveillanceTaskDTO = {
            email: this.createSurveillanceForm.value.email,
            buildingCode: this.createSurveillanceForm.value.buildingCode,
            floorNumber: this.createSurveillanceForm.value.floorNumber,
            contactName: this.createSurveillanceForm.value.contactName,
            contactPhone: this.createSurveillanceForm.value.contactPhone,
        }

        console.log(dto)
        // this.taskService.createSurveillanceTask(dto).subscribe(
    }
}
