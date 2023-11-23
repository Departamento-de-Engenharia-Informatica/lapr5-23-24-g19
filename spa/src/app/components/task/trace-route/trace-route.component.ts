import { Component, EventEmitter, Output } from '@angular/core';
import { FormBuilder, UntypedFormGroup, Validators } from '@angular/forms';
import { catchError, of, tap } from 'rxjs';
import { BuildingDTO } from 'src/app/dto/BuildingDTO';
import { CriteriaDTO } from 'src/app/dto/CriteriaDTO';
import { RouteDTO } from 'src/app/dto/TaskDTO';
import { BuildingService } from 'src/app/services/building.service';
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service';
import { PassageDTO, PassageService } from 'src/app/services/passage.service';
import { RoomDTO } from 'src/app/services/room.service';
import { TaskService } from 'src/app/services/task.service';

@Component({
    selector: 'app-trace-route',
    templateUrl: './trace-route.component.html',
    styleUrls: ['./trace-route.component.css']
})
export class TraceRouteComponent {
    @Output() formSubmitted = new EventEmitter<any>()

    routeForm: UntypedFormGroup

    buildings: BuildingDTO[] = []

    building1!: BuildingDTO
    building2!: BuildingDTO

    floors1!: FloorAndBuildingDTO[]
    floors2!: FloorAndBuildingDTO[]

    rooms2!: RoomDTO[]
    rooms1!: RoomDTO[]

    criterion!: CriteriaDTO[]

    constructor(
        private fb: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,
        private taskService: TaskService,
    ) {
        this.routeForm = this.fb.group({
            building1: [null, Validators.required],
            building2: [null, Validators.required],
            floor1: [null, Validators.required],
            floor2: [null, Validators.required],
            room1: [null, Validators.required],
            room2: [null, Validators.required],
            criteria: [null, Validators.required]
        })
    }

    submitForm() {
        //roomDomainId or building/floor??
        const dto = {
            room1: {
                buildingCode: this.routeForm.value.building1,
                floorNumber: this.routeForm.value.floor1,
                room: this.routeForm.value.room1,
            },
            room2: {
                buildingCode: this.routeForm.value.building2,
                floorNumber: this.routeForm.value.floor2,
                room: this.routeForm.value.room2
            },
        } as RouteDTO

        //TODO: properly implement findRoute()
        this.taskService.findRoute().subscribe((response) => {
            alert("Passage created successfully")
        },
            (error: string) => {
                alert(error)
            },
        )
    }

    onBuilding1Selected(event: any) {
        this.floorService
            .getFloors(event.target.value as string).subscribe((list: FloorAndBuildingDTO[]) => {
                this.floors1 = list
            },
                (error) => {
                    alert(error.message)
                },)
    }

    onBuilding2Selected(event: any) {
        this.floorService
            .getFloors(event.target.value as string)
            .pipe(
                tap((list: FloorAndBuildingDTO[]) => {
                    this.floors2 = list
                }),
                catchError((error) => {
                    if (error.status === 404) {
                        this.floors2 = []
                    } else {
                        // this.message.setErrorMessage(error)
                        console.error('Error fetching floors:', error)
                    }
                    return of()
                }),
            )
            .subscribe()
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((buildingsList: BuildingDTO[]) => {
            this.buildings = buildingsList
        })
        // this.taskService.getCriterion().subscribe((criteria: CriteriaDTO[]) => {
        //     this.criterion = criterion
        // })
        this.criterion = [{ desc: "this" } as CriteriaDTO, { desc: "that" } as CriteriaDTO]
    }

    getBuilding(buildingCode: string): BuildingDTO | undefined {
        return this.buildings.find((building) => building.code === buildingCode)
    }

    isInvalid(controlName: string): boolean {
        const control = this.routeForm.get(controlName)
        return !!control && control.invalid && (control.dirty || control.touched)
    }

    isEmpty(obj: any): boolean {
        return obj != null && obj != undefined && obj.length == 0
    }

    noFloors1(): boolean {
        return this.isEmpty(this.floors1)
    }

    noFloors2(): boolean {
        return this.isEmpty(this.floors2)
    }

    noRooms1(): boolean {
        return this.isEmpty(this.rooms1)
    }
    noRooms2(): boolean {
        return this.isEmpty(this.floors1)
    }
}

