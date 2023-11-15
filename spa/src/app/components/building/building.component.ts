import { Component, EventEmitter, OnInit, Output } from '@angular/core';
import { FormBuilder, UntypedFormGroup, Validators } from '@angular/forms';
import { ActivatedRoute, Data, Router } from '@angular/router';
import { BuildingDTO, BuildingService } from 'src/app/services/building.service';

@Component({
  selector: 'app-building',
  templateUrl: './building.component.html',
  styleUrls: ['./building.component.css']
})


export class BuildingComponent {
  show=false

  onClickCreate(){
    this.show=true
  }

  onClickClose(){
    this.show=false
  }

  
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
