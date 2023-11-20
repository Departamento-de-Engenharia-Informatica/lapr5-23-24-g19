import { Component } from '@angular/core'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import { ErrorMessageService } from 'src/app/services/error-message.service'

@Component({
    selector: 'app-get-buildings',
    templateUrl: './get-buildings.component.html',
    styleUrls: ['./get-buildings.component.css'],
})
export class GetBuildingsComponent {
    private allBuildings: BuildingDTO[] = []

    buildings!: BuildingDTO[]

    searchFilter?: string

    constructor(
        private service: BuildingService,
        private errorSvc: ErrorMessageService,
    ) {}

    ngOnInit() {
        this.listBuildings()
    }

    listBuildings() {
        this.service.getBuildings().subscribe((buildingsList: BuildingDTO[]) => {
            this.allBuildings = buildingsList
            this.buildings = this.allBuildings
        })
    }

    showError(): boolean {
        if (this.buildings != undefined) {
            if (this.buildings.length == 0) {
                this.errorSvc.setErrorMessage('Building Not Found')
                console.log('show')
                return true
            }
        }
        return false
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
