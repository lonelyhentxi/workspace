export class ApplicationDto {
  Name: string;
  Description: string;
  Id: string;
  Avatar: string;
  Url: string;
  Profile: { Id: string, Misc: string, Setting: string };
  Version: string;
}
