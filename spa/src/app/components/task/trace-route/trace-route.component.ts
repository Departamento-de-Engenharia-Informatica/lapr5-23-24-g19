import { Component } from '@angular/core'
import { FormBuilder, UntypedFormGroup, Validators } from '@angular/forms'
import { catchError, of } from 'rxjs'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { CriterionDTO } from 'src/app/dto/CriteriaDTO'
import { BuildingService } from 'src/app/services/building.service'
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service'
import { RoomDTO } from 'src/app/dto/RoomDTO'
import { TaskService } from 'src/app/services/task.service'
import { RoomService } from '../../../services/room.service'
import { GetPathsDTO } from 'src/app/dto/GetPathsDTO'

@Component({
    selector: 'app-trace-route',
    templateUrl: './trace-route.component.html',
    styleUrls: ['./trace-route.component.css'],
})
export class TraceRouteComponent {
    routeForm: UntypedFormGroup

    buildings: BuildingDTO[] = []

    floors1: FloorAndBuildingDTO[] = []
    floors2: FloorAndBuildingDTO[] = []

    rooms1: RoomDTO[] = []
    rooms2: RoomDTO[] = []

    criteria: CriterionDTO[] = []

    constructor(
        private fb: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,
        private roomService: RoomService,
        private taskService: TaskService,
    ) {
        this.routeForm = this.fb.group({
            buildingStart: [null, Validators.required],
            buildingGoal: [null, Validators.required],
            floorStart: [null, Validators.required],
            floorGoal: [null, Validators.required],
            roomStart: [null, Validators.required],
            roomGoal: [null, Validators.required],
            criterion: [null, Validators.required],
        })
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe({
            next: (buildingsList: BuildingDTO[]) => {
                this.buildings = buildingsList
            },
            error: (error) => alert(error),
        })

        this.taskService.getCriteria().subscribe({
            next: (criteria: CriterionDTO[]) => {
                this.criteria = criteria
            },
            error: (error) => alert(error),
        })
    }

    submitForm() {
        const dto: GetPathsDTO = {
            criteria: this.routeForm.value.criterion,
            roomStart: {
                building: this.routeForm.value.buildingStart,
                floor: this.routeForm.value.floorStart,
                name: this.routeForm.value.roomStart,
            },
            roomGoal: {
                building: this.routeForm.value.buildingGoal,
                floor: this.routeForm.value.floorGoal,
                name: this.routeForm.value.roomGoal,
            },
        }

        this.taskService.findRoute(dto).subscribe({
            next: (paths) => {
                alert('Path(s) computed, agora implementai a UI bonita')
                alert(
                    'Isto as vezes demora a computar, por isso uma msg/simbolo de loading n e mal pensado',
                )
                console.log(paths)
            },
            error: (error) => alert(JSON.stringify(error)),
        })
    }

    onBuilding1Selected(event: Event) {
        this.floors1 = []
        this.rooms1 = []

        this.getFloors((event.target as HTMLSelectElement).value).subscribe({
            next: (floors) => {
                this.floors1 = floors
            },
        })
    }

    onBuilding2Selected(event: Event) {
        this.floors2 = []
        this.rooms2 = []

        this.getFloors((event.target as HTMLSelectElement).value).subscribe({
            next: (floors) => {
                this.floors2 = floors
            },
        })
    }

    onFloor1Selected(event: Event) {
        this.rooms1 = []

        const floor = parseInt((event.target as HTMLSelectElement).value)
        this.getRooms(this.routeForm.value.buildingStart, floor).subscribe((rooms) => {
            this.rooms1 = rooms
        })
    }

    onFloor2Selected(event: Event) {
        this.rooms2 = []
        const floor = parseInt((event.target as HTMLSelectElement).value)

        this.getRooms(this.routeForm.value.buildingGoal, floor).subscribe((rooms) => {
            this.rooms2 = rooms
        })
    }

    private getFloors(building: string) {
        return this.floorService.getFloors(building).pipe(
            catchError((error) => {
                console.error(`Error fetching floors: ${error}`)
                return of()
            }),
        )
    }

    private getRooms(building: string, floor: number) {
        return this.roomService.getRooms(building, floor).pipe(
            catchError((error) => {
                console.error('Error fetching floors:', error)
                return of()
            }),
        )
    }
}
