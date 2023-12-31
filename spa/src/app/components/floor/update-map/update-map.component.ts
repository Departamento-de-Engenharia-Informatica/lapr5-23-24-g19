import { Component } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { catchError, of, tap } from 'rxjs'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import {
    FloorAndBuildingDTO,
    FloorService,
    UpdateMapDTO,
} from 'src/app/services/floor.service'

@Component({
    selector: 'app-update-map',
    templateUrl: './update-map.component.html',
    styleUrls: ['./update-map.component.css'],
})
export class UpdateMapComponent {
    updateMapForm: FormGroup
    buildings: BuildingDTO[]
    private allFloors: FloorAndBuildingDTO[]
    floors: FloorAndBuildingDTO[]
    mapFile!: string

    constructor(
        private formBuilder: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,
    ) {
        this.buildings = []
        this.floors = []
        this.allFloors = []

        this.updateMapForm = this.formBuilder.group({
            buildingCode: [null, [Validators.required]],
            floorNumber: [null, [Validators.required]],
            mapFile: [null, [Validators.required]],
        })
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list
        })
    }

    listFloors(): void {
        if (this.updateMapForm.value.buildingCode !== '') {
            this.floorService
                .getFloors(this.updateMapForm.value.buildingCode)
                .pipe(
                    tap((list: FloorAndBuildingDTO[]) => {
                        this.allFloors = list
                        this.floors = this.allFloors
                    }),
                    catchError((error) => {
                        if (error.status === 404) {
                            this.floors = []
                        } else {
                            console.error('Error fetching floors:', error)
                        }
                        return of()
                    }),
                )
                .subscribe()
        }
    }

    hasFloors(): boolean {
        return this.floors && this.floors.length > 0
    }

    onFileSelected(event: Event): void {
        const fileInput = event.target as HTMLInputElement
        const file = fileInput?.files?.[0]

        if (file) {
            const fileReader = new FileReader()

            fileReader.onload = (fileEvent) => {
                if (fileEvent.target) {
                    const fileContent = fileEvent.target.result as string
                    this.mapFile = fileContent
                }
            }
            fileReader.readAsText(file)
        }
    }

    submitForm() {
        const dto: UpdateMapDTO = JSON.parse(this.mapFile)

        console.log('mapFile', this.mapFile)
        console.log('dto', dto)
        this.floorService
            .updateMap(
                dto,
                this.updateMapForm.value.buildingCode,
                this.updateMapForm.value.floorNumber,
            )
            .subscribe(
                (map: UpdateMapDTO) => {
                    alert('Map updated successfully')
                },
                (error) => {
                    alert(JSON.stringify(error))
                },
            )
    }

    checkValid() {
        return this.mapFile != null && this.mapFile != undefined && this.mapFile != ''
    }
}
