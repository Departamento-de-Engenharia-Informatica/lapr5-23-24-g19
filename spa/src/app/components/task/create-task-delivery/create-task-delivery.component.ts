import { Component } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { AuthService } from '@auth0/auth0-angular'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { CreateDeliveryTaskDTO } from '../../../../../../mdr/src/dto/CreateDeliveryTaskDTO'
import { RoomDTO } from 'src/app/dto/RoomDTO'
import { BuildingService } from 'src/app/services/building.service'
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service'
import { RoomService } from 'src/app/services/room.service'
import { TaskService } from 'src/app/services/task.service'

@Component({
    selector: 'app-create-task-delivery',
    templateUrl: './create-task-delivery.component.html',
    styleUrls: ['./create-task-delivery.component.css'],
})
export class CreateTaskDeliveryComponent {
    selectedBuilding1: string = ''
    selectedFloor1: number = null as unknown as number
    selectedRoom1: string = ''

    selectedBuilding2: string = ''
    selectedFloor2: number = null as unknown as number
    selectedRoom2: string = ''

    createDeliveryForm: FormGroup = null as unknown as FormGroup

    buildings: BuildingDTO[] = []

    floors1: FloorAndBuildingDTO[] = []
    floors2: FloorAndBuildingDTO[] = []

    rooms1: RoomDTO[] = []
    rooms2: RoomDTO[] = []

    constructor(
        private formBuilder: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,
        private roomService: RoomService,
        private taskService: TaskService,
        public authService: AuthService,
    ) {
        this.createDeliveryForm = this.formBuilder.group({
            email: ['', [Validators.required]],

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

            description: ['', [Validators.maxLength(1000)]],
            confirmationCode: [this.generateConfirmationCode(), [Validators.required]],
        })
    }

    ngOnInit(): void {
        this.authService.isAuthenticated$.subscribe((isAuthenticated) => {
            if (isAuthenticated) {
                console.log('Authenticated')
                this.authService.user$.subscribe((user) => {
                    this.createDeliveryForm.controls['email'].setValue(user?.email)
                })

                this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
                    this.buildings = list
                })
            }
        })
    }

    generateConfirmationCode(): number {
        return Math.floor(Math.random() * (999999 - 100000 + 1)) + 100000
    }

    listFloors1(): void {
        if (this.selectedBuilding1.length !== 0) {
            this.createDeliveryForm.controls['startFloorNumber'].setValue(null)
            this.createDeliveryForm.controls['startRoom'].setValue(null)

            this.floorService
                .getFloors(this.selectedBuilding1)
                .subscribe((list: FloorAndBuildingDTO[]) => {
                    this.rooms1 = []
                    this.floors1 = list
                })
        }
    }

    listFloors2(): void {
        if (this.selectedBuilding2.length !== 0) {
            this.createDeliveryForm.controls['goalFloorNumber'].setValue(null)
            this.createDeliveryForm.controls['goalRoom'].setValue(null)

            this.floorService
                .getFloors(this.selectedBuilding2)
                .subscribe((list: FloorAndBuildingDTO[]) => {
                    this.rooms2 = []
                    this.floors2 = list
                })
        }
    }

    listRooms1(): void {
        if (this.selectedFloor1 !== null) {
            this.createDeliveryForm.controls['startRoom'].setValue(null)

            this.roomService
                .getRooms(this.selectedBuilding1, this.selectedFloor1)
                .subscribe((list: RoomDTO[]) => {
                    this.rooms1 = list.filter((room: RoomDTO) => {
                        return room.name !== this.createDeliveryForm.value.goalRoom
                    })
                })
        }
    }

    listRooms2(): void {
        if (this.selectedFloor2 !== null) {
            this.createDeliveryForm.controls['goalRoom'].setValue(null)

            this.roomService
                .getRooms(this.selectedBuilding2, this.selectedFloor2)
                .subscribe((list: RoomDTO[]) => {
                    this.rooms2 = list.filter((room: RoomDTO) => {
                        return room.name !== this.createDeliveryForm.value.startRoom
                    })
                })
        }
    }

    changedRoom2(): void {
        if (this.selectedFloor1 !== null) {
            if (
                this.createDeliveryForm.controls['startRoom'] ===
                this.createDeliveryForm.controls['goalRoom']
            ) {
                this.createDeliveryForm.controls['startRoom'].setValue(null)
            }

            this.roomService
                .getRooms(this.selectedBuilding1, this.selectedFloor1)
                .subscribe((list: RoomDTO[]) => {
                    this.rooms1 = list.filter((room: RoomDTO) => {
                        return room.name !== this.createDeliveryForm.value.goalRoom
                    })
                })
        }
    }

    changedRoom1(): void {
        if (this.selectedFloor2 !== null) {
            if (
                this.createDeliveryForm.controls['startRoom'] ===
                this.createDeliveryForm.controls['goalRoom']
            ) {
                this.createDeliveryForm.controls['goalRoom'].setValue(null)
            }

            this.roomService
                .getRooms(this.selectedBuilding2, this.selectedFloor2)
                .subscribe((list: RoomDTO[]) => {
                    this.rooms2 = list.filter((room: RoomDTO) => {
                        return room.name !== this.createDeliveryForm.value.startRoom
                    })
                })
        }
    }

    onSubmit(): void {
        const dto: CreateDeliveryTaskDTO = {
            email: this.createDeliveryForm.value.email,
            startBuildingCode: this.createDeliveryForm.value.startBuildingCode,
            startFloorNumber: this.createDeliveryForm.value.startFloorNumber,
            startRoom: this.createDeliveryForm.value.startRoom,
            goalBuildingCode: this.createDeliveryForm.value.goalBuildingCode,
            goalFloorNumber: this.createDeliveryForm.value.goalFloorNumber,
            goalRoom: this.createDeliveryForm.value.goalRoom,
            pickupContactName: this.createDeliveryForm.value.pickupContactName,
            pickupContactPhone: this.createDeliveryForm.value.pickupContactPhone,
            deliveryContactName: this.createDeliveryForm.value.deliveryContactName,
            deliveryContactPhone: this.createDeliveryForm.value.deliveryContactPhone,
            confirmationCode: this.createDeliveryForm.value.confirmationCode,
        }

        const description = this.createDeliveryForm.value.description
        if (description !== '') dto.description = description

        console.log(dto)
        this.taskService.createDeliveryTask(dto).subscribe(
            (task: CreateDeliveryTaskDTO) => {
                let alertMessage = `Task created successfully!`
                alertMessage += `\nEmail: ${dto.email}`
                alertMessage += `\nStart building: ${dto.startBuildingCode}`
                alertMessage += `\nStart floor: ${dto.startFloorNumber}`
                alertMessage += `\nStart room: ${dto.startRoom}`
                alertMessage += `\nGoal building: ${dto.goalBuildingCode}`
                alertMessage += `\nGoal floor: ${dto.goalFloorNumber}`
                alertMessage += `\nGoal room: ${dto.goalRoom}`
                alertMessage += `\nPickup contact name: ${dto.pickupContactName}`
                alertMessage += `\nPickup contact phone: ${dto.pickupContactPhone}`
                alertMessage += `\nDelivery contact name: ${dto.deliveryContactName}`
                alertMessage += `\nDelivery contact phone: ${dto.deliveryContactPhone}`

                if (description !== '') alertMessage += `\nDescription: ${description}`

                alert(alertMessage)

                this.createDeliveryForm.reset({
                    email: this.createDeliveryForm.value.email,
                    confirmationCode: this.generateConfirmationCode(),
                })
            },
            (error) => {
                alert(error.error)
                this.createDeliveryForm.reset({
                    email: this.createDeliveryForm.value.email,
                    confirmationCode: this.generateConfirmationCode(),
                })
            },
        )
    }
}
