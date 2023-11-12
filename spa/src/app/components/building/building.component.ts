import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Data, Router } from '@angular/router';
import { BuildingDTO } from 'src/app/services/building.service';

@Component({
  selector: 'app-building',
  templateUrl: './building.component.html',
  styleUrls: ['./building.component.css']
})


export class BuildingComponent{
  // selectedBuilding!: BuildingDTO;

  // onBuildingEdited(editedBuilding: BuildingDTO) {
  //   // Handle the edited building, e.g., update it in the list or perform any other action
  //   console.log('Building Edited:', editedBuilding);
  //   this.selectedBuilding = {} as BuildingDTO; // Clear the selected building after editing
  // }
}
