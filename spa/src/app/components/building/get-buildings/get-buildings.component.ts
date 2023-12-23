import { Component } from '@angular/core'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'

@Component({
    selector: 'app-get-buildings',
    templateUrl: './get-buildings.component.html',
    styleUrls: ['./get-buildings.component.css'],
})
export class GetBuildingsComponent {
    private allBuildings: BuildingDTO[] = []

    buildings!: BuildingDTO[]

    searchFilter?: string

    constructor(private service: BuildingService) {}

    ngOnInit() {
        this.listBuildings()
    }

    listBuildings() {
        this.service.getBuildings().subscribe(
            (buildingsList: BuildingDTO[]) => {
                this.allBuildings = buildingsList
                this.buildings = this.allBuildings
                if (this.buildings.length == 0) {
                    alert('No buildings Found')
                }
            },
            (error) => {
                alert(error)
            },
        )
    }
    filter(event: Event) {
        const prop = (event.target as HTMLInputElement).value.trim().toLowerCase()
        if (prop.length === 0) {
            this.buildings = this.allBuildings
        } else {
            this.buildings = this.allBuildings.filter(
                (b) =>
                    b.code.toLowerCase().includes(prop) ||
                    (prop.length > 2 &&
                        (b.name?.toLowerCase().includes(prop) ||
                            b.description?.toLowerCase().includes(prop))),
            )
        }
    }
}
