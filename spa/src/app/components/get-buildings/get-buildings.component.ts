import { Component, EventEmitter, Input, Output } from '@angular/core';
import { FormBuilder, FormGroupDirective, FormGroupName, UntypedFormGroup, Validators } from '@angular/forms';
import { BuildingService,BuildingDTO } from 'src/app/services/building.service';

@Component({
  selector: 'app-get-buildings',
  templateUrl: './get-buildings.component.html',
  styleUrls: ['./get-buildings.component.css']
})
export class GetBuildingsComponent {
  buildings: BuildingDTO[] = []
  
  constructor(private fb: FormBuilder, private service: BuildingService) {
    service.getBuildings().subscribe((buildingsList: BuildingDTO[]) => {
      this.buildings = buildingsList;
      });
  }
}

