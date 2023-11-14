import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { BuildingComponent } from './components/building/building.component';
import { RobotComponent } from './components/robot/robot.component';
import { FloorComponent } from './components/floor/floor.component';
import { PassageComponent } from './components/passage/passage.component';
import { PageNotFoundComponent } from './components/page-not-found/page-not-found.component';
import { RobotTypeComponent } from './components/robot-type/robot-type.component';
import { ElevatorComponent } from './components/elevator/elevator.component';
import { RoomComponent } from './components/room/room.component';
import { Visualization3DComponent } from './components/visualization3-d/visualization3-d.component';
import { AppComponent } from './app.component';
import { MainMenuComponent } from './components/main-menu/main-menu.component';
import { ListFloorsComponent } from './components/floor/list-floors/list-floors.component';
import { GetBuildingsComponent } from './components/get-buildings/get-buildings.component';
import { ModulesComponent } from './components/modules/modules.component';
import { EditBuildingComponent } from './components/edit-building/edit-building.component';
import { CreateElevatorComponent } from './components/create-elevator/create-elevator.component';

export const routes: Routes = [
    { path: '', component: ModulesComponent, title: 'Home page' },
    { path: 'modules', component: ModulesComponent, title: 'Modules page' },
    { path: 'menu', component: MainMenuComponent, title: 'Home page' },
    { path: 'buildings', component: BuildingComponent, title: 'Buildings' },
    {
        path: 'buildings/edit',
        component: EditBuildingComponent,
        title: 'Edit Building',
    },
    {
        path: 'buildings/list',
        component: GetBuildingsComponent,
        title: 'List Buildings',
    },
    // children: [
    //   { path: 'list', component: GetBuildingsComponent,title: 'List of Buildings'},
    // ]},
    //TODO: Properly route each component
    { path: 'floors', component: FloorComponent, title: 'Floors' },
    {
        path: 'floors/list',
        component: ListFloorsComponent,
        title: 'List Floors',
    },
    { path: 'elevators', component: ElevatorComponent },
    {
        path: 'elevators/create',
        component: CreateElevatorComponent,
        title: 'Create Elevator',
    },
    { path: 'rooms', component: RoomComponent },
    { path: 'passages', component: PassageComponent },
    { path: 'robot-types', component: RobotTypeComponent },
    { path: 'robots', component: RobotComponent },
    { path: '3D-visualization', component: Visualization3DComponent },
    { path: 'campus', component: MainMenuComponent, title: 'Home page' },
    { path: '**', component: PageNotFoundComponent },
];

@NgModule({
    imports: [RouterModule.forRoot(routes)],
    exports: [RouterModule],
})
export class AppRoutingModule {}
