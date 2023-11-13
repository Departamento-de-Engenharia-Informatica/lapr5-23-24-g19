import { Component, EventEmitter, Output } from '@angular/core';
import { FormBuilder, UntypedFormGroup, Validators } from '@angular/forms';
import { BuildingDTO, BuildingService } from 'src/app/services/building.service';

@Component({
  selector: 'app-create-elevator',
  templateUrl: './create-elevator.component.html',
  styleUrls: ['./create-elevator.component.css']
})
export class CreateElevatorComponent {

  @Output() formSubmitted = new EventEmitter<any>();
  buildingForm: UntypedFormGroup
  buildings: BuildingDTO[] = []


  constructor(private fb: FormBuilder, private service: BuildingService) {
    service.getBuildings().subscribe((buildingsList: BuildingDTO[]) => {
      this.buildings = buildingsList;
      });
    this.buildingForm = this.fb.group({
      selectedBuildingCode: [null, Validators.required],
      name: [''],
      description: [''],
      length: [null, [Validators.required, Validators.min(0)]],
      width: [null, [Validators.required, Validators.min(0)]]
    });
  }

  submitForm() {
    if (this.buildingForm.valid) {
      this.formSubmitted.emit(this.buildingForm.value);
      this.buildingForm.reset();
    }
  }

  onFileSelected(event: any) {
    const file = event.target.files[0];
    this.buildingForm.patchValue({ file });
  }

}
