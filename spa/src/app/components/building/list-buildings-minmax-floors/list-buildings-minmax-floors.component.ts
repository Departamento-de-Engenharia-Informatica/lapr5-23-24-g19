import { Component } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import {
    BuildingByFloorsDTO,
    BuildingService,
    MinMaxDTO,
} from 'src/app/services/building.service';

@Component({
    selector: 'app-list-buildings-minmax-floors',
    templateUrl: './list-buildings-minmax-floors.component.html',
    styleUrls: ['./list-buildings-minmax-floors.component.css'],
})
export class ListBuildingsMinmaxFloorsComponent {
    private allBuildings: BuildingByFloorsDTO[] = [];
    buildings: BuildingByFloorsDTO[] = [];
    filterForm: FormGroup = null as unknown as FormGroup;

    constructor(
        private formBuilder: FormBuilder,
        private service: BuildingService,
    ) {}

    ngOnInit() {
        this.filterForm = this.formBuilder.group({
            min: [null, [Validators.min(1), Validators.required]],
            max: [null, [Validators.min(1), Validators.required]],
        });
    }

    onSubmit(): void {
        const dto: MinMaxDTO = {
            min: this.filterForm.value.min as unknown as number,
            max: this.filterForm.value.max as unknown as number,
        };

        this.service
            .getBuildingsByFloors(dto)
            .subscribe((list: BuildingByFloorsDTO[]) => {
                this.allBuildings = list;
                this.buildings = this.allBuildings;
            });
    }

    filter(event: Event) {
        const prop = (event.target as HTMLInputElement).value
            .trim()
            .toLowerCase();
        if (prop.length === 0) {
            this.buildings = this.allBuildings;
        } else {
            this.buildings = this.allBuildings.filter(
                (b) =>
                    b.code.toLowerCase().includes(prop) ||
                    (prop.length > 2 &&
                        (b.name?.toLowerCase().includes(prop) ||
                            b.description?.toLowerCase().includes(prop))),
            );
        }
    }

    submitDisabled(): boolean {
        const min = this.filterForm.value.min as unknown as number;
        const max = this.filterForm.value.max as unknown as number;
        return min < 1 || max < 1 || min > max;
    }

    isInvalid(controlName: string): boolean {
        const control = this.filterForm.get(controlName);
        return (
            !!control && control.invalid && (control.dirty || control.touched)
        );
    }
}