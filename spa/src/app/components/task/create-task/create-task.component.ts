import { Component } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { CreateDeliveryTaskDTO } from 'src/app/dto/CreateDeliveryTaskDTO'
import { CreateSurveillanceTaskDTO } from 'src/app/dto/CreateSurveillanceTaskDTO'
import { BuildingService } from 'src/app/services/building.service'
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service'

@Component({
    selector: 'app-create-task',
    templateUrl: './create-task.component.html',
    styleUrls: ['./create-task.component.css'],
})
export class CreateTaskComponent {
    selectedBuilding: string = ''
    selectedFloor: number = null as unknown as number

    createSurveillanceForm: FormGroup = null as unknown as FormGroup
    createDeliveryForm: FormGroup = null as unknown as FormGroup

    buildings: BuildingDTO[] = []
    floors: FloorAndBuildingDTO[] = []

    constructor(
        private formBuilder: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,
    ) {
        this.createSurveillanceForm = this.formBuilder.group({
            email: [null, [Validators.required]],

            buildingCode: [null, [Validators.required]],
            floorNumber: [null, [Validators.required]],

            contactName: [null, [Validators.required]],
            contactPhone: [null, [Validators.required]],
        })

        this.createDeliveryForm = this.formBuilder.group({
            email: [null, [Validators.required]],

            startBuildingCode: [null, [Validators.required]],
            startFloorNumber: [null, [Validators.required]],
            startRoom: [null, [Validators.required]],

            goalBuildingCode: [null, [Validators.required]],
            goalFloorNumber: [null, [Validators.required]],
            goalRoom: [null, [Validators.required]],

            pickupContactName: [null, [Validators.required]],
            pickupContactPhone: [null, [Validators.required]],

            deliveryContactName: [null, [Validators.required]],
            deliveryContactPhone: [null, [Validators.required]],

            description: '',
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

    onSubmitSurveillance(): void {
        const dto: CreateSurveillanceTaskDTO = {
            email: this.createSurveillanceForm.value.email,
            buildingCode: this.createSurveillanceForm.value.buildingCode,
            floorNumber: this.createSurveillanceForm.value.floorNumber,
            contactName: this.createSurveillanceForm.value.contactName,
            contactPhone: this.createSurveillanceForm.value.contactPhone,
        }

        console.log(dto)
        // call service to do the request
    }

    onSubmitDelivery(): void {
        const dto: CreateDeliveryTaskDTO = {
            email: this.createSurveillanceForm.value.email,
            startBuildingCode: this.createSurveillanceForm.value.startBuildingCode,
            startFloorNumber: this.createSurveillanceForm.value.startFloorNumber,
            startRoom: this.createSurveillanceForm.value.startRoom,
            goalBuildingCode: this.createSurveillanceForm.value.goalBuildingCode,
            goalFloorNumber: this.createSurveillanceForm.value.goalFloorNumber,
            goalRoom: this.createSurveillanceForm.value.goalRoom,
            pickupContactName: this.createSurveillanceForm.value.pickupContactName,
            pickupContactPhone: this.createSurveillanceForm.value.pickupContactPhone,
            deliveryContactName: this.createSurveillanceForm.value.deliveryContactName,
            deliveryContactPhone: this.createSurveillanceForm.value.deliveryContactPhone,
        }

        const description = this.createDeliveryForm.value.description
        if (description !== '') dto.description = description

        console.log(dto)
        // call service to do the request
    }
}
