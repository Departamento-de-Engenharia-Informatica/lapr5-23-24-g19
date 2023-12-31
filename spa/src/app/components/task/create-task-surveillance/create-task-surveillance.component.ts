import { Component } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { AuthService } from '@auth0/auth0-angular'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { CreateSurveillanceTaskDTO } from '../../../../../../mdr/src/dto/CreateSurveillanceTaskDTO'
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
        private taskService: TaskService,
        public authService: AuthService,
    ) {
        this.createSurveillanceForm = this.formBuilder.group({
            email: ['', [Validators.required]],

            buildingCode: [null, [Validators.required]],
            floorNumber: [null, [Validators.required]],

            contactName: [null, [Validators.required]],
            contactPhone: [null, [Validators.required]],
        })
    }

    ngOnInit(): void {
        this.authService.isAuthenticated$.subscribe((isAuthenticated) => {
            if (isAuthenticated) {
                console.log('Authenticated')
                this.authService.user$.subscribe((user) => {
                    this.createSurveillanceForm.controls['email'].setValue(user?.email)
                })

                this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
                    this.buildings = list
                })
            }
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
        this.taskService.createSurveillanceTask(dto).subscribe(
            (task: CreateSurveillanceTaskDTO) => {
                let alertMessage = `Task created successfully!`
                alertMessage += `\nBuilding: ${dto.buildingCode}`
                alertMessage += `\nFloor: ${dto.floorNumber}`
                alertMessage += `\nContact Name: ${dto.contactName}`
                alertMessage += `\nContact Phone: ${dto.contactPhone}`

                alert(alertMessage)

                this.createSurveillanceForm.reset({
                    email: this.createSurveillanceForm.value.email,
                })
            },
            (error) => {
                alert(error.error)
                this.createSurveillanceForm.reset({
                    email: this.createSurveillanceForm.value.email,
                })
            },
        )
    }
}
