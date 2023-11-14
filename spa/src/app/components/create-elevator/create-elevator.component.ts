import { Component, EventEmitter, Output } from '@angular/core';
import { FormBuilder, UntypedFormGroup, Validators } from '@angular/forms';
import {
    BuildingDTO,
    BuildingService,
} from 'src/app/services/building.service';
import {
    ElevatorDTO,
    ElevatorService,
} from 'src/app/services/elevator.service';
import { FloorDTO, FloorService } from 'src/app/services/floor.service';

@Component({
    selector: 'app-create-elevator',
    templateUrl: './create-elevator.component.html',
    styleUrls: ['./create-elevator.component.css'],
})
export class CreateElevatorComponent {
    @Output() formSubmitted = new EventEmitter<any>();
    elevatorForm: UntypedFormGroup;
    buildings: BuildingDTO[] = [];

    floors: Floor[] = [];
    elevators: ElevatorDTO[] = [];

    constructor(
        private fb: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,
        private elevatorService: ElevatorService,
    ) {
        this.elevatorForm = this.fb.group({
            selectedBuildingCode: ['', Validators.required],
            selectedFloors: [[], Validators.required],
            identifier: ['', Validators.required],
            brand: ['', Validators.required],
            model: ['', Validators.required],
            serialNumber: ['', Validators.required],
            description: [''],
        });

        buildingService
            .getBuildings()
            .subscribe((buildingsList: BuildingDTO[]) => {
                this.buildings = buildingsList;
            });

        this.elevatorForm
            .get('selectedBuildingCode')
            ?.valueChanges.subscribe((selectedBuildingCode) => {
                if (
                    selectedBuildingCode !== null &&
                    selectedBuildingCode !== undefined
                ) {
                    this.floorService
                        .getFloors(selectedBuildingCode)
                        .subscribe((floorsList: Floor[]) => {
                            this.floors = floorsList;
                        });
                } else {
                    this.floors = []; // Clear floors if no building is selected
                }
            });
    }

    submitElevatorForm() {
        /*if (this.elevatorForm.valid) {
            this.formSubmitted.emit(this.elevatorForm.value);
            this.elevatorForm.reset();
        }*/

        if (this.elevatorForm.valid) {
            // Set values to the properties
            const selectedBuildingCode = this.elevatorForm.get(
                'selectedBuildingCode',
            )?.value;
            const selectedFloors =
                this.elevatorForm.get('selectedFloors')?.value;
            const identifier = this.elevatorForm.get('identifier')?.value;
            const brand = this.elevatorForm.get('brand')?.value;
            const model = this.elevatorForm.get('model')?.value;
            const serialNumber = this.elevatorForm.get('serialNumber')?.value;
            const description = this.elevatorForm.get('description')?.value;

            // Emit the form values
            this.formSubmitted.emit({
                selectedBuildingCode: selectedBuildingCode,
                selectedFloors: selectedFloors,
                identifier: identifier,
                brand: brand,
                model: model,
                serialNumber: serialNumber,
                description: description,
            });

            // Reset the form
            this.elevatorForm.reset();
        }
    }

    /*
  submitForm() {
    if (this.buildingForm.valid) {
      this.formSubmitted.emit(this.buildingForm.value);
      this.buildingForm.reset();
    }
  }

  onFileSelected(event: any) {
    const file = event.target.files[0];
    this.buildingForm.patchValue({ file });
  }*/
}
